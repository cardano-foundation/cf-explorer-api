package org.cardanofoundation.explorer.api.service.impl;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.mapper.DRepCertificateMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.drep.DRepCertificateHistoryResponse;
import org.cardanofoundation.explorer.api.model.response.drep.projection.DRepCertificateProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.DRepRegistrationRepository;
import org.cardanofoundation.explorer.api.service.DRepService;

@Service
@RequiredArgsConstructor
public class DRepServiceImpl implements DRepService {

  private final DRepRegistrationRepository dRepRegistrationRepository;
  private final DRepCertificateMapper dRepCertificateMapper;

  @Override
  public BaseFilterResponse<DRepCertificateHistoryResponse> getTxDRepCertificateHistory(
      String drepHashOrDrepId, Pageable pageable) {
    List<DRepCertificateProjection> dRepCertificateProjections =
        dRepRegistrationRepository.getDRepCertificateByDRepIdOrHash(drepHashOrDrepId);
    List<DRepCertificateHistoryResponse> dRepCertificateHistoryResponses =
        dRepCertificateProjections.stream()
            .collect(Collectors.groupingBy(DRepCertificateProjection::getTxHash))
            .values()
            .stream()
            .map(
                dRepCertificateProjectionList -> {
                  DRepCertificateHistoryResponse dRepCertificateHistoryResponse;
                  dRepCertificateHistoryResponse =
                      dRepCertificateMapper.fromDRepCertProjection(
                          dRepCertificateProjectionList.get(0));
                  dRepCertificateHistoryResponse.setActionTypes(
                      dRepCertificateProjectionList.stream()
                          .map(DRepCertificateProjection::getType)
                          .toList());
                  return dRepCertificateHistoryResponse;
                })
            .sorted(
                Sort.Direction.DESC.equals(
                        pageable.getSort().getOrderFor("createdAt").getDirection())
                    ? Comparator.comparing(DRepCertificateHistoryResponse::getCreatedAt).reversed()
                    : Comparator.comparing(DRepCertificateHistoryResponse::getCreatedAt))
            .toList();

    return new BaseFilterResponse<>(
        BaseFilterResponse.getPageImpl(dRepCertificateHistoryResponses, pageable));
  }
}
