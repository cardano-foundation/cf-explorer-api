package com.cardano.explorer.view;

import java.math.BigDecimal;
import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.annotation.Immutable;

@Entity
@Immutable
@Table(name = "address_token_input")
@Getter
@Setter
public class AddressTokenInput {

  @EmbeddedId
  private AddressTokenInputId id;

  @Column(name = "token_name")
  private String tokenName;

  @Column(name = "quantity")
  private BigDecimal quantity;

}
