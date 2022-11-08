package com.cardano.explorer.common.converter;

import com.cardano.explorer.common.enumeration.RewardType;
import javax.persistence.AttributeConverter;
import javax.persistence.Converter;

@Converter(autoApply = true)
public class RewardTypeConverter implements AttributeConverter<RewardType, String> {

  @Override
  public String convertToDatabaseColumn(RewardType attribute) {
    return attribute.getValue();
  }

  @Override
  public RewardType convertToEntityAttribute(String dbData) {
    return RewardType.fromValue(dbData);
  }
}
