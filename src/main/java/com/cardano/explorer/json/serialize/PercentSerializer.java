package com.cardano.explorer.json.serialize;

import com.cardano.explorer.common.constant.CommonConstant;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;

public class PercentSerializer extends JsonSerializer<Double> {

  @Override
  public void serialize(Double value, JsonGenerator gen, SerializerProvider serializers)
      throws IOException {
    gen.writeNumber(BigDecimal.valueOf(value).setScale(CommonConstant.SCALE, RoundingMode.HALF_UP));
  }
}
