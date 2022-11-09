package com.cardano.explorer.common.converter;

import com.cardano.explorer.common.enumeration.SyncStateType;
import javax.persistence.AttributeConverter;
import javax.persistence.Converter;

@Converter(autoApply = true)
public class SyncStateTypeConverter implements AttributeConverter<SyncStateType, String> {

  @Override
  public String convertToDatabaseColumn(SyncStateType attribute) {
    return attribute.getValue();
  }

  @Override
  public SyncStateType convertToEntityAttribute(String dbData) {
    return SyncStateType.fromValue(dbData);
  }
}
