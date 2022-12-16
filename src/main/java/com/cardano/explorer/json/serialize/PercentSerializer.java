package com.cardano.explorer.json.serialize;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;

public class PercentSerializer extends JsonSerializer<Double> {

  private static final int SCALE = 5;

  @Override
  public void serialize(Double value, JsonGenerator gen, SerializerProvider serializers)
      throws IOException {
    gen.writeNumber(BigDecimal.valueOf(value).setScale(SCALE, RoundingMode.HALF_UP));
  }
}
