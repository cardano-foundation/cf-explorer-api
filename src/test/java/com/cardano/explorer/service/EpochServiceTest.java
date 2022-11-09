package com.cardano.explorer.service;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.assertj.core.api.AssertionsForClassTypes.assertThatThrownBy;
import static org.mockito.Mockito.when;

import com.cardano.explorer.exception.BusinessException;
import com.cardano.explorer.repository.EpochRepository;
import com.cardano.explorer.service.impl.EpochServiceImpl;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class EpochServiceTest {

  @InjectMocks
  private EpochServiceImpl epochServiceMock;

  @Autowired
  private EpochService epochService;

  @Mock
  private EpochRepository epochRepository;

  @Test
  void whenGetEpochWithValidEpochNo_shouldReturnEpoch() {
    var response = epochService.getEpochDetail(2);
    assertThat(response.getNo()).isEqualTo(2);
  }

  @Test
  void whenGetEpochWithInvalidEpochNo_shouldThrow() {
    assertThatThrownBy(() -> epochService.getEpochDetail(-1)).isInstanceOf(BusinessException.class);
  }

  @Test
  void whenGetCurrentEpoch_shouldReturnEpochNoCurrent() {
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.of(200));
    var response =
        epochService.getCurrentEpoch();
    assertThat(response).isNotNull();
  }

//  @Test
//  void whenGetEpochWithPage_shouldReturnEpochList() {
//    var response =
//        epochService.filterEpoch(PageRequest.of(0,20, Sort.by(Direction.DESC, "no")));
//    assertThat(response.getData().size()).isSameAs(20);
//  }

  @Test
  void whenGetCurrentEpochInvalid_shouldThrow() {
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.empty());
    assertThatThrownBy(() -> epochServiceMock.getCurrentEpoch()).isInstanceOf(BusinessException.class);
  }

  @Test
  void whenGetEpochWithPageWithCurrentEpochNul_shouldThrow() {
    when(epochRepository.findCurrentEpochNo()).thenReturn(Optional.empty());
    assertThatThrownBy(
        () -> epochServiceMock.filterEpoch(PageRequest.of(0,20, Sort.by(Direction.DESC, "no"))))
        .isInstanceOf(BusinessException.class);
  }

}
