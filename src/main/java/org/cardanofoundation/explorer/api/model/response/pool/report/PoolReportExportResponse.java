package org.cardanofoundation.explorer.api.model.response.pool.report;

import java.io.ByteArrayInputStream;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class PoolReportExportResponse {
  private String fileName;
  private ByteArrayInputStream byteArrayInputStream;
}
