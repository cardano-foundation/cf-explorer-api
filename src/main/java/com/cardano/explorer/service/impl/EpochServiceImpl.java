package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.enumeration.EpochStatus;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.EpochMapper;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.EpochResponse;
import com.cardano.explorer.model.response.dashboard.EpochSummary;
import com.cardano.explorer.repository.EpochRepository;
import com.cardano.explorer.service.EpochService;
import com.sotatek.cardano.common.entity.Epoch;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class EpochServiceImpl implements EpochService {

  public static final int MILLI = 1000;
  private final EpochRepository epochRepository;
  private final EpochMapper epochMapper;

  @Override
  @Transactional(readOnly = true)
  public EpochResponse getEpochDetail(String no) {
    try {
      Integer epochNo = Integer.parseInt(no);
      Epoch epoch = epochRepository.findFirstByNo(epochNo).orElseThrow(
          () -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND)
      );
      EpochResponse response = epochMapper.epochToEpochResponse(epoch);
      var rewardTime = LocalDateTime.now().minusDays(10);
      var currentEpoch = epochRepository.findCurrentEpochNo().orElseThrow(
          () -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND));
      if (currentEpoch.equals(response.getNo())) {
        response.setStatus(EpochStatus.IN_PROGRESS);
      } else if (rewardTime.isBefore(response.getEndTime())) {
        response.setStatus(EpochStatus.REWARDING);
      } else {
        response.setStatus(EpochStatus.FINISHED);
      }
      return response;
    } catch (NumberFormatException e) {
      throw new BusinessException(BusinessCode.EPOCH_NOT_FOUND);
    }
  }

  @Override
  @Transactional(readOnly = true)
  public Integer getCurrentEpoch() {
    return epochRepository.findCurrentEpochNo().orElseThrow(
        () -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND)
    );
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<EpochResponse> filterEpoch(Pageable pageable) {
    Page<Epoch> epochs = epochRepository.findAll(pageable);
    var rewardTime = LocalDateTime.now(ZoneId.of("UTC")).minusDays(10);
    var currentEpoch = epochRepository.findCurrentEpochNo().orElseThrow(
        () -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND));
    Page<EpochResponse> pageResponse = epochs.map(epochMapper::epochToEpochResponse);
    pageResponse.getContent().forEach(epoch -> {
      if (currentEpoch.equals(epoch.getNo())) {
        epoch.setStatus(EpochStatus.IN_PROGRESS);
      } else if (rewardTime.isBefore(epoch.getEndTime())) {
        epoch.setStatus(EpochStatus.REWARDING);
      } else {
        epoch.setStatus(EpochStatus.FINISHED);
      }
    });

    return new BaseFilterResponse<>(pageResponse);
  }

  @Override
  @Transactional(readOnly = true)
  public EpochSummary getCurrentEpochSummary() {

    return epochRepository
        .findCurrentEpochSummary()
        .map(epochSummaryProjection -> {
          var slot =
              (Instant.now().toEpochMilli() - epochSummaryProjection.getStartTime().getTime())
                  / MILLI;
          return EpochSummary.builder()
              .no(epochSummaryProjection.getNo())
              .slot((int) slot)
              .totalSlot(epochSummaryProjection.getMaxSlot())
              .build();
        })
        .orElse(EpochSummary.builder().slot(0).no(0).totalSlot(0).build());
  }
}
