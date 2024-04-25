package org.cardanofoundation.explorer.api.service;

import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.explorer.api.repository.ledgersync.TxRepository;

@ExtendWith(MockitoExtension.class)
public class InstantaneousRewardsServiceTest {

  @Mock private TxRepository txRepository;
}
