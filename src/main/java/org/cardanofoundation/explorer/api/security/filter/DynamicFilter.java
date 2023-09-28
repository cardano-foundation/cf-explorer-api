package org.cardanofoundation.explorer.api.security.filter;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.AllArgsConstructor;
import org.cardanofoundation.explorer.api.mapper.RoleFilterMapper;
import org.cardanofoundation.explorer.api.security.utility.RoleConfiguration;
import org.cardanofoundation.explorer.common.utils.JwtUtils;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.HashMap;

@Component
@AllArgsConstructor
public class DynamicFilter extends OncePerRequestFilter {

    private final RoleConfiguration roleConfiguration;

    @Override
    public void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) throws IOException, ServletException {

        HashMap<String, RoleFilterMapper> roleConf = roleConfiguration.getRoleConfiguration();

        if (roleConf.toString().contains(request.getRequestURI())) {
            String jwt = JwtUtils.parseJwt(request);

            if(jwt != null){
                String checkRequest = request.getAttribute("role").toString() + "/" + request.getRequestURI();
                RoleFilterMapper roleFilter = roleConf.get(checkRequest);

                //TO-DO: Save history to storage then check with configuration
                if(null != roleFilter){
                    response.sendError(HttpStatus.UNAUTHORIZED.value(), "Unauthorized request");
                }
            }

        } else {
            //"Filter NOT intercepted";
        }

        filterChain.doFilter(request, response);
    }

}