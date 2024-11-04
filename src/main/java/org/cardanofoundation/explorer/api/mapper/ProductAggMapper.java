package org.cardanofoundation.explorer.api.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import org.cardanofoundation.cf_product_tracing_aggregator.ProductAggregationRecord;
import org.cardanofoundation.explorer.api.model.response.BolnisiProjectNumberResponse;

@Mapper(componentModel = "spring")
public interface ProductAggMapper {

  @Mapping(target = "numberOfCertificates", source = "productAggregationRecord.certificates")
  @Mapping(target = "numberOfWineries", source = "productAggregationRecord.producers")
  @Mapping(target = "numberOfBottles", source = "productAggregationRecord.units")
  BolnisiProjectNumberResponse toBolnisiProjectResponse(
      ProductAggregationRecord productAggregationRecord);
}
