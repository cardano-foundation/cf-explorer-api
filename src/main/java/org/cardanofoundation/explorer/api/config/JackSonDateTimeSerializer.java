package org.cardanofoundation.explorer.api.config;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class JackSonDateTimeSerializer extends JsonSerializer<LocalDateTime> {

  public static final String DATE_TIME_FORMAT = "yyyy/MM/dd HH:mm:ss";

  /**
   * Method that can be called to ask implementation to serialize values of type this serializer
   * handles.
   *
   * @param value Value to serialize; can <b>not</b> be null.
   * @param gen Generator used to output resulting Json content
   * @param serializers Provider that can be used to get serializers for serializing Objects value
   *     contains, if any.
   */
  @Override
  public void serialize(LocalDateTime value, JsonGenerator gen, SerializerProvider serializers)
      throws IOException {
    gen.writeString(value.format(DateTimeFormatter.ofPattern(DATE_TIME_FORMAT)));
  }
}
