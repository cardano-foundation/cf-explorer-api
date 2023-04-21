package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.enumeration.ProtocolType;
import com.cardano.explorer.model.response.protocol.ProtocolHistory;
import com.cardano.explorer.repository.ParamProposalRepository;
import com.cardano.explorer.service.ProtocolParamService;
import com.sotatek.cardano.common.entity.ParamProposal;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.annotation.PostConstruct;
import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

@Service
@Log4j2
@FieldDefaults(level = AccessLevel.PRIVATE)
@RequiredArgsConstructor
public class ProtocolServiceImpl implements ProtocolParamService {

  final ParamProposalRepository paramProposalRepository;
  Map<String, Method> paramProtocolMethod;

  @Override
  public Set<ProtocolHistory> getProtocolHistory(ProtocolType protocolType) {
    Stream<ParamProposal> historiesChange =
        paramProposalRepository.getAllDistinctProtocolParam(0l)
            .stream();
    if (protocolType.equals(ProtocolType.COST_MODEL)) {
      return historiesChange
          .filter(paramProposal -> Objects.nonNull(paramProposal.getCostModel()))
          .map(paramProposal ->
              ProtocolHistory
                  .builder()
                  .value(paramProposal.getCostModel().getCosts())
                  .transactionHash(paramProposal.getRegisteredTx().getHash())
                  .time(paramProposal.getRegisteredTx().getBlock().getTime())
                  .build())
          .collect(Collectors.toCollection(LinkedHashSet::new));
    }

    Stream<ProtocolHistory> historyStream;
    historyStream = getHistoryStream(historiesChange, paramProposal -> {
      try {
        return paramProtocolMethod.get(protocolType.getFieldName()).invoke(paramProposal);
      } catch (IllegalAccessException | InvocationTargetException e) {
        log.error(e.getMessage());
        return null;
      }
    });

    return historyStream.collect(Collectors.toCollection(LinkedHashSet::new));
  }

  private Stream<ProtocolHistory> getHistoryStream(Stream<ParamProposal> historiesChange,
      Function<ParamProposal, ?> function) {

    return historiesChange
        .filter(paramProposal -> Objects.nonNull(function.apply(paramProposal)))
        .map(paramProposal ->
            ProtocolHistory
                .builder()
                .value(function.apply(paramProposal))
                .transactionHash(paramProposal.getRegisteredTx().getHash())
                .time(paramProposal.getRegisteredTx().getBlock().getTime())
                .build()
        );
  }

  @PostConstruct
  public void setup() {
    paramProtocolMethod = new HashMap<>();
    Field[] fields = ParamProposal.class.getDeclaredFields();
    Method[] methods = ParamProposal.class.getDeclaredMethods();

    for (Field field : fields) {
      Method methodUsed = Arrays.stream(methods)
          .filter(method -> method.getName().toLowerCase().contains(field.getName().toLowerCase()))
          .findFirst()
          .orElse(null);
      if (Objects.nonNull(methodUsed)) {
        paramProtocolMethod.put(field.getName(), methodUsed);
      }
    }
  }
}
