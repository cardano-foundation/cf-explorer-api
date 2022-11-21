package com.cardano.explorer.repository.custom;

import com.cardano.explorer.entity.Epoch;

public interface CustomEpochRepository extends CustomRepository<Epoch> {

  Epoch findByCurrentEpochNo();
}
