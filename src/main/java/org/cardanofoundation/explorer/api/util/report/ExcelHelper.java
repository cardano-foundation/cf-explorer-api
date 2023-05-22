package org.cardanofoundation.explorer.api.util.report;

import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.util.DataUtil;
import org.cardanofoundation.explorer.api.util.ReflectorUtil;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import lombok.extern.log4j.Log4j2;
import org.apache.poi.hssf.usermodel.HSSFFont;
import org.apache.poi.ss.usermodel.*;

import java.io.ByteArrayInputStream;
import java.lang.reflect.Field;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.*;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

@Log4j2
public class ExcelHelper {

  private static final String DATE_TIME_PATTERN = "yyyy/MM/dd HH:mm:ss";

  public static ByteArrayInputStream writeContent(List<ExportContent> exportContents) {
    try {
      Workbook workbook = new XSSFWorkbook();
      CellStyle cellStyleHeader = createStyleHeader(workbook);

      for (ExportContent exportContent : exportContents) {
        Sheet sheet = workbook.createSheet(exportContent.getHeaderTitle());
        List<ExportColumn> lstColumn = exportContent.getLstColumn();
        List<?> lstData = exportContent.getLstData();
        int startRow = 0;

        // write header
        Row rowHeader = sheet.createRow(startRow);
        for (int i = 0; i < lstColumn.size(); i++) {
          Cell cell = rowHeader.createCell(i);
          cell.setCellValue(lstColumn.get(i).getColumnTitle().getValue());
          cell.setCellStyle(cellStyleHeader);
          sheet.autoSizeColumn(i);
        }
        writeDataReport(workbook, exportContent, sheet, lstColumn, lstData);
      }
      ByteArrayOutputStream out = new ByteArrayOutputStream();
      workbook.write(out);
      return new ByteArrayInputStream(out.toByteArray());
    } catch (final IOException | IllegalAccessException e) {
      log.error("Excel writing error: ", e);
      throw new BusinessException(BusinessCode.INTERNAL_ERROR);
    }
  }

  private static void writeDataReport(Workbook workbook, ExportContent exportContent, Sheet sheet,
      List<ExportColumn> lstColumn, List<?> lstData) throws IllegalAccessException {
    List<Field> fields = ReflectorUtil.getAllFields(exportContent.getClazz());
    Map<String, Field> mapField = new HashMap<>();

    exportContent.getLstColumn()
        .forEach(exportColumn -> fields.stream()
            .peek(f -> f.setAccessible(true))
            .filter(f -> f.getName().equals(exportColumn.getColumnField().getValue()))
            .forEach(f -> mapField.put(exportColumn.getColumnField().getValue(), f)));

    if(DataUtil.isNullOrEmpty(lstData)){
      Row row = sheet.createRow(1);
      Cell cell = row.createCell(0);
      CellStyle cellStyle = workbook.createCellStyle();
      cellStyle.setAlignment(HorizontalAlignment.CENTER);
      Font font = workbook.createFont();
      font.setFontName(HSSFFont.FONT_ARIAL);
      font.setFontHeightInPoints((short) 11);
      cellStyle.setFont(font);
      cell.setCellStyle(cellStyle);
      cell.setCellValue("No records");
      sheet.addMergedRegion(new CellRangeAddress(1, 1, 0, lstColumn.size() - 1));
      return;
    }

    for (int i = 0; i < lstData.size(); i++) {
      Object obj = lstData.get(i);
      Row row = sheet.createRow(i + 1);

      for (int j = 0; j < lstColumn.size(); j++) {
        Cell cell = row.createCell(j);
        ExportColumn exportColumn = lstColumn.get(j);
        Field field = mapField.get(exportColumn.getColumnField().getValue());
        if (field == null) {
          log.error("Field not found: " + exportColumn.getColumnField().getValue());
          throw new BusinessException(BusinessCode.INTERNAL_ERROR);
        }
        Object value = field.get(obj);
        String text;
        if (value instanceof Double) {
          text = DataUtil.doubleToString((Double) value);
        } else if (value instanceof Instant) {
          text = DataUtil.instantToString((Instant) value, DATE_TIME_PATTERN);
        } else if (value instanceof Timestamp) {
          text = DataUtil.instantToString(((Timestamp) value).toInstant(), DATE_TIME_PATTERN);
        } else if (value instanceof Date) {
          text = DataUtil.instantToString(((Date) value).toInstant(), DATE_TIME_PATTERN);
        } else if (value instanceof LocalDateTime) {
          text = DataUtil.instantToString(
              ((LocalDateTime) value).atZone(ZoneOffset.UTC).toInstant(),
              DATE_TIME_PATTERN);
        } else {
          text = DataUtil.objectToString(value);
        }
        cell.setCellValue(text);
        int numberOfLines = text.split("\n").length;
        row.setHeightInPoints(numberOfLines * sheet.getDefaultRowHeightInPoints());
        sheet.autoSizeColumn(j);
        switch (exportColumn.getAlign()){
          case LEFT:
            cell.setCellStyle(createCellStyle(workbook, HorizontalAlignment.LEFT));
            break;
          case RIGHT:
            cell.setCellStyle(createCellStyle(workbook, HorizontalAlignment.RIGHT));
            break;
          case CENTER:
            cell.setCellStyle(createCellStyle(workbook, HorizontalAlignment.CENTER));
            break;
        }
      }
    }
  }

  private static CellStyle createCellStyle(Workbook workbook, HorizontalAlignment horizontalAlignment) {
    CellStyle cellStyle = workbook.createCellStyle();
    cellStyle.setAlignment(horizontalAlignment);
    cellStyle.setVerticalAlignment(VerticalAlignment.CENTER);
    cellStyle.setBorderLeft(BorderStyle.THIN);
    cellStyle.setBorderBottom(BorderStyle.THIN);
    cellStyle.setBorderRight(BorderStyle.THIN);
    cellStyle.setBorderTop(BorderStyle.THIN);
    cellStyle.setWrapText(true);
    cellStyle.setDataFormat((short) BuiltinFormats.getBuiltinFormat("@"));

    Font font = workbook.createFont();
    font.setFontName(HSSFFont.FONT_ARIAL);
    font.setFontHeightInPoints((short) 11);
    cellStyle.setFont(font);
    return cellStyle;
  }

  private static CellStyle createStyleHeader(Workbook workbook) {
    CellStyle cellStyleHeader = createCellStyleHeader(workbook);
    Font fontHeader = workbook.createFont();
    fontHeader.setFontName(HSSFFont.FONT_ARIAL);
    fontHeader.setBold(true);
    fontHeader.setFontHeightInPoints((short) 11);
    cellStyleHeader.setFont(fontHeader);
    return cellStyleHeader;
  }

  private static CellStyle createCellStyleHeader(Workbook workbook) {
    CellStyle cellStyleHeader = workbook.createCellStyle();
    cellStyleHeader.setAlignment(HorizontalAlignment.CENTER);
    cellStyleHeader.setVerticalAlignment(VerticalAlignment.CENTER);
    cellStyleHeader.setBorderLeft(BorderStyle.THIN);
    cellStyleHeader.setBorderBottom(BorderStyle.THIN);
    cellStyleHeader.setBorderRight(BorderStyle.THIN);
    cellStyleHeader.setBorderTop(BorderStyle.THIN);
    cellStyleHeader.setFillForegroundColor(IndexedColors.GREY_25_PERCENT.index);
    cellStyleHeader.setFillPattern(FillPatternType.SOLID_FOREGROUND);
    cellStyleHeader.setWrapText(true);
    return cellStyleHeader;
  }
}
