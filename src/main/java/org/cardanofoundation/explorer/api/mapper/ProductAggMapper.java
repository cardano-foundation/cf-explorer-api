package org.cardanofoundation.explorer.api.mapper;

import com.example.api.model.ProductAggregationRecord;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import org.cardanofoundation.explorer.api.model.response.BolnisiProjectNumberResponse;

@Mapper(componentModel = "spring")
public interface ProductAggMapper {

  @Mapping(target = "certificates", source = "productAggregationRecord.certificates")
  @Mapping(target = "producers", source = "productAggregationRecord.producers")
  @Mapping(target = "units", source = "productAggregationRecord.units")
  BolnisiProjectNumberResponse toBolnisiProjectResponse(
      ProductAggregationRecord productAggregationRecord);
}
