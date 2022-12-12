package com.cardano.explorer.view;

import java.math.BigDecimal;
import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.annotation.Immutable;

@Entity
@Immutable
@Table(name = "address_token_output")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class AddressTokenOutput {

  @EmbeddedId
  private AddressTokenOutputId id;

  @Column(name = "token_name")
  private String tokenName;

  @Column(name = "quantity")
  private BigDecimal quantity;

}
