package org.cardanofoundation.explorer.api.mapper;

import com.example.api.model.ProductAggregationRecord;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import org.cardanofoundation.explorer.api.model.response.BolnisiProjectNumberResponse;

@Mapper(componentModel = "spring")
public interface ProductAggMapper {

  @Mapping(target = "numberOfCertificates", source = "productAggregationRecord.certificates")
  @Mapping(target = "numberOfWineries", source = "productAggregationRecord.producers")
  @Mapping(target = "numberOfBottles", source = "productAggregationRecord.units")
  BolnisiProjectNumberResponse toBolnisiProjectResponse(
      ProductAggregationRecord productAggregationRecord);
}
