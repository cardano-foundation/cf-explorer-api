package org.cardanofoundation.explorer.api.model.response.pool.report;

import lombok.Builder;
import lombok.Data;

import java.io.ByteArrayInputStream;

@Data
@Builder
public class PoolReportExportResponse {
  private String fileName;
  private ByteArrayInputStream byteArrayInputStream;
}
