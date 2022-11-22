package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.TxIn;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TxInRepository extends JpaRepository<TxIn, Long> {

}
