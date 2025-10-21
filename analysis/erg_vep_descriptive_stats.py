#!/usr/bin/env python3
"""Generate descriptive statistics for ERG and VEP datasets.

This script parses the Excel exports that hold flash and flicker ERG/VEP
measurements without relying on external dependencies.  It reports standard
summary statistics (count, mean, standard deviation, quartiles, extrema) for
all numeric ERG/VEP metrics and writes the results to a Markdown file for
convenient review.
"""
from __future__ import annotations

import argparse
import math
import os
import statistics
import sys
import xml.etree.ElementTree as ET
from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Sequence, Tuple
from zipfile import ZipFile

SPREADSHEET_NS = "{http://schemas.openxmlformats.org/spreadsheetml/2006/main}"


@dataclass
class DescriptiveStats:
    """Container for descriptive statistics for a numeric series."""

    count: int
    mean: float
    std: Optional[float]
    minimum: float
    q1: Optional[float]
    median: float
    q3: Optional[float]
    maximum: float

    def as_row(self) -> List[Optional[float]]:
        """Return the statistics in table-friendly order."""

        return [
            self.count,
            self.mean,
            self.std,
            self.minimum,
            self.q1,
            self.median,
            self.q3,
            self.maximum,
        ]


def read_shared_strings(zf: ZipFile) -> List[str]:
    """Extract the workbook's shared strings table."""

    try:
        data = zf.read("xl/sharedStrings.xml")
    except KeyError:
        return []

    root = ET.fromstring(data)
    strings: List[str] = []
    for si in root.findall(f"{SPREADSHEET_NS}si"):
        parts = [t.text or "" for t in si.iter(f"{SPREADSHEET_NS}t")]
        strings.append("".join(parts))
    return strings


def column_index(cell_reference: str) -> int:
    """Convert an Excel column name to a zero-based index."""

    letters = "".join(ch for ch in cell_reference if ch.isalpha())
    index = 0
    for ch in letters:
        index = index * 26 + (ord(ch.upper()) - ord("A") + 1)
    return index - 1


def read_sheet(path: str, sheet_name: str = "sheet1") -> List[List[Optional[object]]]:
    """Read the requested worksheet into a list of rows.

    Only built-in modules are used so the script can run in constrained
    environments where third-party Excel parsers are unavailable.
    """

    with ZipFile(path) as zf:
        shared_strings = read_shared_strings(zf)
        try:
            sheet_data = zf.read(f"xl/worksheets/{sheet_name}.xml")
        except KeyError as exc:  # pragma: no cover - sheet name mismatch
            raise ValueError(f"worksheet '{sheet_name}' not found in {path}") from exc

    root = ET.fromstring(sheet_data)

    rows: Dict[int, Dict[int, Optional[object]]] = {}
    max_col = -1
    max_row = 0

    for row in root.iter(f"{SPREADSHEET_NS}row"):
        row_index = int(row.attrib["r"])
        cells: Dict[int, Optional[object]] = {}
        for cell in row.findall(f"{SPREADSHEET_NS}c"):
            ref = cell.attrib.get("r", "")
            col_index = column_index(ref)
            max_col = max(max_col, col_index)
            cell_type = cell.attrib.get("t")
            value = None
            value_element = cell.find(f"{SPREADSHEET_NS}v")

            if cell_type == "s":
                if value_element is not None and value_element.text is not None:
                    shared_index = int(value_element.text)
                    value = shared_strings[shared_index] if shared_index < len(shared_strings) else ""
                else:
                    value = ""
            elif cell_type == "b":
                value = bool(int(value_element.text)) if value_element is not None and value_element.text else False
            elif cell_type == "inlineStr":
                texts = [t_el.text or "" for t_el in cell.iter(f"{SPREADSHEET_NS}t")]
                value = "".join(texts)
            else:
                if value_element is not None and value_element.text is not None:
                    text_value = value_element.text
                    try:
                        if any(ch in text_value for ch in (".", "E", "e")):
                            value = float(text_value)
                        else:
                            value = int(text_value)
                    except ValueError:
                        value = text_value
                else:
                    value = None

            cells[col_index] = value
        rows[row_index] = cells
        max_row = max(max_row, row_index)

    # Assemble the dense table, filling missing cells with None.
    dense_rows: List[List[Optional[object]]] = []
    for row_index in range(1, max_row + 1):
        cell_map = rows.get(row_index, {})
        dense_rows.append([cell_map.get(col_index) for col_index in range(max_col + 1)])

    return dense_rows


def rows_to_records(rows: Sequence[Sequence[Optional[object]]]) -> Tuple[List[str], List[Dict[str, Optional[object]]]]:
    """Convert worksheet rows into dictionaries keyed by their header."""

    if not rows:
        raise ValueError("worksheet is empty")

    header = [str(column) if column is not None else f"column_{index}" for index, column in enumerate(rows[0])]
    records: List[Dict[str, Optional[object]]] = []
    for row in rows[1:]:
        if row is None:
            continue
        record = {header[index]: row[index] if index < len(row) else None for index in range(len(header))}
        # Skip completely empty rows at the bottom of the sheet.
        if all(value is None or (isinstance(value, str) and not value.strip()) for value in record.values()):
            continue
        records.append(record)
    return header, records


def coerce_numeric(value: Optional[object]) -> Optional[float]:
    """Convert a cell value to a float when possible."""

    if value is None:
        return None
    if isinstance(value, (int, float)):
        return float(value)
    if isinstance(value, str) and value.strip():
        try:
            return float(value)
        except ValueError:
            return None
    return None


def compute_statistics(values: Iterable[float]) -> Optional[DescriptiveStats]:
    """Compute descriptive statistics for a collection of floats."""

    series = [value for value in values if value is not None and not math.isnan(value)]
    if not series:
        return None

    count = len(series)
    mean = statistics.fmean(series)
    std = statistics.stdev(series) if count > 1 else None
    minimum = min(series)
    maximum = max(series)
    median = statistics.median(series)
    q1 = q3 = None
    if count >= 2:
        quartiles = statistics.quantiles(series, n=4, method="inclusive")
        q1, _, q3 = quartiles

    return DescriptiveStats(count, mean, std, minimum, q1, median, q3, maximum)


def collect_numeric_columns(header: Sequence[str], records: Sequence[Dict[str, Optional[object]]]) -> Dict[str, List[float]]:
    """Extract numeric data series for each column."""

    numeric_columns: Dict[str, List[float]] = {}
    for column in header:
        if column == "SubjectID":
            continue
        series: List[float] = []
        for record in records:
            numeric_value = coerce_numeric(record.get(column))
            if numeric_value is not None:
                series.append(numeric_value)
        if series:
            numeric_columns[column] = series
    return numeric_columns


def group_columns(columns: Iterable[str]) -> Dict[str, List[str]]:
    """Group columns into ERG, VEP, or Other categories."""

    groups: Dict[str, List[str]] = {"ERG": [], "VEP": [], "Other": []}
    for column in columns:
        if "ERG" in column:
            groups["ERG"].append(column)
        elif "VEP" in column:
            groups["VEP"].append(column)
        else:
            groups["Other"].append(column)
    return {group: sorted(names) for group, names in groups.items() if names}


def format_number(value: Optional[float]) -> str:
    """Render a numeric value with sensible precision."""

    if value is None:
        return ""
    if isinstance(value, int) or value.is_integer():
        return f"{int(value)}"
    formatted = f"{value:,.3f}"
    if "." in formatted:
        formatted = formatted.rstrip("0").rstrip(".")
    return formatted


def build_markdown_table(rows: List[Tuple[str, DescriptiveStats]]) -> str:
    """Create a Markdown table summarising statistics for each metric."""

    headers = [
        "Metric",
        "Count",
        "Mean",
        "Std",
        "Min",
        "Q1",
        "Median",
        "Q3",
        "Max",
    ]
    lines = ["| " + " | ".join(headers) + " |", "|" + "---|" * len(headers)]

    for metric, stats in rows:
        values = stats.as_row()
        formatted_values = [
            format_number(values[0]),
            format_number(values[1]),
            format_number(values[2]),
            format_number(values[3]),
            format_number(values[4]),
            format_number(values[5]),
            format_number(values[6]),
            format_number(values[7]),
        ]
        line = f"| {metric} | " + " | ".join(formatted_values) + " |"
        lines.append(line)
    return "\n".join(lines)


def analyse_workbook(path: str) -> Dict[str, Dict[str, DescriptiveStats]]:
    """Compute descriptive statistics for every numeric column in a workbook."""

    rows = read_sheet(path)
    header, records = rows_to_records(rows)
    numeric_columns = collect_numeric_columns(header, records)

    results: Dict[str, Dict[str, DescriptiveStats]] = {}
    for group, columns in group_columns(numeric_columns.keys()).items():
        group_stats: Dict[str, DescriptiveStats] = {}
        for column in columns:
            stats = compute_statistics(numeric_columns[column])
            if stats is not None:
                group_stats[column] = stats
        if group_stats:
            results[group] = group_stats
    return results


def write_markdown_report(
    flash_stats: Dict[str, Dict[str, DescriptiveStats]],
    flicker_stats: Dict[str, Dict[str, DescriptiveStats]],
    output_path: str,
) -> None:
    """Persist the descriptive statistics as a Markdown report."""

    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    sections: List[str] = [
        "# ERG and VEP Descriptive Statistics",
        "",
        "This report summarises the ERG and VEP measurements extracted from",
        "`AllSubjectsResultsFlash.xlsx` and `AllSubjectsResultsFlicker.xlsx`.",
    ]

    def append_sections(title: str, stats: Dict[str, Dict[str, DescriptiveStats]]) -> None:
        sections.extend(["", f"## {title}", ""])
        for group, metrics in stats.items():
            sections.extend([f"### {group} metrics", ""])
            table_rows = sorted(metrics.items())
            sections.append(build_markdown_table(table_rows))
            sections.append("")

    append_sections("Flash stimulus", flash_stats)
    append_sections("Flicker stimulus", flicker_stats)

    with open(output_path, "w", encoding="utf-8") as handle:
        handle.write("\n".join(sections).rstrip() + "\n")


def parse_arguments(argv: Optional[Sequence[str]] = None) -> argparse.Namespace:
    """Parse command-line arguments."""

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--flash",
        default="AllSubjectsResultsFlash.xlsx",
        help="Path to the flash ERG results workbook.",
    )
    parser.add_argument(
        "--flicker",
        default="AllSubjectsResultsFlicker.xlsx",
        help="Path to the flicker ERG/VEP results workbook.",
    )
    parser.add_argument(
        "--output",
        default="analysis/erg_vep_descriptive_stats.md",
        help="Where to store the Markdown report.",
    )
    return parser.parse_args(argv)


def main(argv: Optional[Sequence[str]] = None) -> int:
    """Script entry point."""

    args = parse_arguments(argv)

    flash_stats = analyse_workbook(args.flash)
    flicker_stats = analyse_workbook(args.flicker)
    write_markdown_report(flash_stats, flicker_stats, args.output)

    print(f"Descriptive statistics saved to {args.output}")
    return 0


if __name__ == "__main__":  # pragma: no cover - direct execution
    sys.exit(main())
