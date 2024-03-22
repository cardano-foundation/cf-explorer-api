package org.cardanofoundation.explorer.api.service;

import static org.mockito.Mockito.when;

import java.util.List;

import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;

import org.mapstruct.factory.Mappers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.explorer.api.mapper.DRepCertificateMapper;
import org.cardanofoundation.explorer.api.model.response.drep.projection.DRepCertificateProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.DRepRegistrationRepository;
import org.cardanofoundation.explorer.api.service.impl.DRepServiceImpl;
import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.DRepActionType;

@ExtendWith(MockitoExtension.class)
public class DRepCertificateServiceTest {
  @Mock DRepRegistrationRepository dRepRegistrationRepository;
  @InjectMocks DRepServiceImpl dRepCertificateService;

  @Spy
  private DRepCertificateMapper dRepCertificateMapper =
      Mappers.getMapper(DRepCertificateMapper.class);

  @Test
  public void testGetDRepCertificateHistory() {
    String drepHash = "3fad80b8e41ed700e63b9967a3f5664b7dbe79e7385e392b4940ed73";
    DRepCertificateProjection dRepCertificateProjection1 =
        Mockito.mock(DRepCertificateProjection.class);
    when(dRepCertificateProjection1.getTxHash())
        .thenReturn("947e3c4acb552d04ad22259768d96420f43b8d354048ae596f2439fc9c099fca");
    when(dRepCertificateProjection1.getTxIndex()).thenReturn(0L);
    when(dRepCertificateProjection1.getEpochNo()).thenReturn(10);
    when(dRepCertificateProjection1.getAbsoluteSlot()).thenReturn(1000L);
    when(dRepCertificateProjection1.getBlockNo()).thenReturn(100L);
    when(dRepCertificateProjection1.getSlotNo()).thenReturn(50L);
    when(dRepCertificateProjection1.getType()).thenReturn(DRepActionType.REG_DREP_CERT);

    DRepCertificateProjection dRepCertificateProjection2 =
        Mockito.mock(DRepCertificateProjection.class);
    when(dRepCertificateProjection2.getTxHash())
        .thenReturn("947e3c4acb552d04ad22259768d96420f43b8d354048ae596f2439fc9c099fca");
    when(dRepCertificateProjection2.getType()).thenReturn(DRepActionType.UPDATE_DREP_CERT);

    when(dRepRegistrationRepository.getDRepCertificateByDRepIdOrHash(drepHash))
        .thenReturn(List.of(dRepCertificateProjection1, dRepCertificateProjection2));

    var actual =
        dRepCertificateService.getTxDRepCertificateHistory(
            drepHash, PageRequest.of(0, 2, Sort.by("createdAt").descending()));

    Assertions.assertEquals(1, actual.getTotalItems());
    Assertions.assertEquals(
        actual.getData().get(0).getActionTypes(),
        List.of(DRepActionType.REG_DREP_CERT, DRepActionType.UPDATE_DREP_CERT));
  }
}
