package org.cardanofoundation.explorer.api.json.serialize;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;

import org.cardanofoundation.explorer.api.common.constant.CommonConstant;

public class PercentSerializer extends JsonSerializer<Double> {

  @Override
  public void serialize(Double value, JsonGenerator gen, SerializerProvider serializers)
      throws IOException {
    gen.writeNumber(BigDecimal.valueOf(value).setScale(CommonConstant.SCALE, RoundingMode.HALF_UP));
  }
}
