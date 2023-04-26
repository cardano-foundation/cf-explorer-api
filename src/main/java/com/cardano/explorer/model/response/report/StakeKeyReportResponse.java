package com.cardano.explorer.model.response.report;

import java.io.ByteArrayInputStream;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class StakeKeyReportResponse {
  private String fileName;
  private ByteArrayInputStream byteArrayInputStream;
}
