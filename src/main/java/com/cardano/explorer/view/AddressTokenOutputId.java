package com.cardano.explorer.view;

import java.io.Serializable;
import javax.persistence.Column;
import javax.persistence.Embeddable;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Embeddable
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class AddressTokenOutputId implements Serializable {

  @Column(name = "address")
  private String address;

  @Column(name = "fingerprint")
  private String fingerprint;
}
