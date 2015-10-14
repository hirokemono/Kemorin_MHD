!
!      module int_surf_div_fluxes_sgs
!
!      Written by H. Matsui on Sep., 2005
!
!!      subroutine int_surf_div_h_flux_sgs(sf_grp, n_int)
!!      subroutine int_surf_div_m_flux_sgs(sf_grp, n_int)
!!      subroutine int_surf_lorentz_sgs(sf_grp, n_int,                  &
!!     &          i_filter, i_tensor, i_vect, i_scalar)
!!      subroutine int_surf_commute_sgs_m_flux(sf_grp, n_int,           &
!!     &          i_filter, i_flux, i_vect)
!!      subroutine int_surf_commute_sgs_maxwell(sf_grp, n_int,          &
!!     &          i_filter, i_flux, i_vect)
!!      subroutine int_surf_commute_sgs_h_flux(sf_grp, n_int,           &
!!     &          i_filter, i_flux, i_vect, i_scalar)
!
      module int_surf_div_fluxes_sgs
!
      use m_precision
!
      use m_physical_property
      use t_group_data
      use check_finite_element_mat
!
      implicit none
!
      private :: int_sf_skv_sgs_div_v_flux, int_sf_skv_sgs_div_t_flux
      private :: int_sf_skv_commute_sgs_v_flux
      private :: int_sf_skv_commute_sgs_t_flux
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_div_h_flux_sgs(sf_grp, n_int)
!
      use m_control_parameter
      use m_node_phys_address
      use m_SGS_model_coefs
      use m_SGS_address
      use m_surf_data_temp
!
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind=kint), intent(in) :: n_int
!
!
      call int_sf_skv_sgs_div_v_flux(sf_grp, n_int, ngrp_sf_sgs_temp,   &
     &    id_grp_sf_sgs_temp, ifilter_final, iphys%i_SGS_h_flux,        &
     &    iphys%i_velo, iphys%i_temp, ak_diff(1,iak_diff_hf),           &
     &    coef_temp)
!
      end subroutine int_surf_div_h_flux_sgs
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_div_m_flux_sgs(sf_grp, n_int)
!
      use m_control_parameter
      use m_node_phys_address
      use m_SGS_model_coefs
      use m_SGS_address
      use m_surf_data_torque
!
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind=kint), intent(in) :: n_int
!
!
      call int_sf_skv_sgs_div_t_flux(sf_grp, n_int, nmax_sf_sgs_velo,   &
     &    ngrp_sf_sgs_velo, id_grp_sf_sgs_velo, ifilter_final,          &
     &    iphys%i_SGS_m_flux, iphys%i_velo, iphys%i_velo,               &
     &    ak_diff(1,iak_diff_mf), coef_velo)
!
      end subroutine int_surf_div_m_flux_sgs
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_lorentz_sgs(sf_grp, n_int,                    &
     &          i_filter, i_tensor, i_vect, i_scalar)
!
      use m_SGS_model_coefs
      use m_SGS_address
      use m_surf_data_magne
!
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer(kind=kint), intent(in) :: i_vect, i_scalar, i_tensor
!
!
      type(surface_group_data), intent(in) :: sf_grp
      call int_sf_skv_sgs_div_t_flux(sf_grp, n_int,                     &
     &    nmax_sf_sgs_magne, ngrp_sf_sgs_magne, id_grp_sf_sgs_magne,    &
     &    i_filter, i_tensor, i_vect, i_scalar,                         &
     &    ak_diff(1,iak_diff_lor), (-coef_lor) )
!
      end subroutine int_surf_lorentz_sgs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_surf_commute_sgs_m_flux(sf_grp, n_int,             &
     &          i_filter, i_flux, i_vect)
!
      use m_surf_data_torque
!
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind = kint), intent(in) :: n_int, i_filter
      integer(kind = kint), intent(in) :: i_flux, i_vect
!
!
      call int_sf_skv_commute_sgs_t_flux(sf_grp, n_int,                 &
     &    nmax_sf_sgs_velo, ngrp_sf_sgs_velo, id_grp_sf_sgs_velo,       &
     &    i_filter, i_flux, i_vect, i_vect)
!
      end subroutine int_surf_commute_sgs_m_flux
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_commute_sgs_maxwell(sf_grp, n_int,            &
     &          i_filter, i_flux, i_vect)
!
      use m_surf_data_magne
!
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind = kint), intent(in) :: n_int, i_filter
      integer(kind = kint), intent(in) :: i_flux, i_vect
!
!
      call int_sf_skv_commute_sgs_t_flux(sf_grp, n_int,                 &
     &    nmax_sf_sgs_magne, ngrp_sf_sgs_magne, id_grp_sf_sgs_magne,    &
     &    i_filter, i_flux, i_vect, i_vect)
!
      end subroutine int_surf_commute_sgs_maxwell
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_commute_sgs_h_flux(sf_grp, n_int,             &
     &          i_filter, i_flux, i_vect, i_scalar)
!
      use m_surf_data_temp
!
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind = kint), intent(in) :: n_int, i_filter
      integer(kind = kint), intent(in) :: i_flux, i_vect, i_scalar
!
!
       call int_sf_skv_commute_sgs_v_flux                               &
     &    (sf_grp, n_int, ngrp_sf_sgs_temp,                             &
     &     id_grp_sf_sgs_temp, i_filter,  i_flux, i_vect, i_scalar)
!
      end subroutine int_surf_commute_sgs_h_flux
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_sf_skv_sgs_div_v_flux(sf_grp, n_int, ngrp_sf,      &
     &          id_grp_sf, i_filter, i_tensor, i_vect, i_scalar,        &
     &          ak_diff, coef_field)
!
      use m_geometry_data
      use m_sorted_node
      use m_finite_element_matrix
      use m_int_surface_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_filter_elength
!
      use delta_SGS_2_each_surface
      use fem_surf_skv_sgs_commute_t
      use cal_skv_to_ff_smp
!
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind=kint), intent(in) :: ngrp_sf
      integer(kind=kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer(kind=kint), intent(in) :: i_vect, i_scalar, i_tensor
      real (kind = kreal), intent(in) :: ak_diff(ele1%numele)
      real (kind = kreal), intent(in) :: coef_field
!
      integer(kind=kint) :: k2, i, igrp, num
!
!
      if (ngrp_sf .eq. 0) return
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
! --------- set vector at each node in an element
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
        num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
!
        if (num .gt. 0) then
          do k2 = 1, surf1%nnod_4_surf
            call d_SGS_flux_2_each_sf_w_cst(sf_grp, igrp, k2, ione,     &
     &          i_vect, i_scalar, i_tensor, dminus, vect_sf)
            call fem_sf_grp_skv_sgs_div_flux_p                          &
     &         (ele1, surf1, sf_grp, jac1_sf_grp_2d_q, FEM1_elen,       &
     &          igrp, k2, ione, n_int, i_filter, dxe_sf, vect_sf,       &
     &          ak_diff, coef_field, fem1_wk%sk6)
          end do
!
        end if
      end do
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_sf_skv_sgs_div_v_flux
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_skv_sgs_div_t_flux(sf_grp, n_int,               &
     &          nmax_sf, ngrp_sf, id_grp_sf, i_filter,                  &
     &          i_tensor, i_vect, i_scalar, ak_diff, coef_field)
!
      use m_geometry_data
      use m_sorted_node
      use m_finite_element_matrix
      use m_int_surface_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_filter_elength
!
      use delta_SGS_2_each_surface
      use fem_surf_skv_sgs_commute_t
      use cal_skv_to_ff_smp
!
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind=kint), intent(in) :: nmax_sf
      integer(kind=kint), intent(in) :: ngrp_sf(3)
      integer(kind=kint), intent(in) :: id_grp_sf(nmax_sf,3)
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer(kind=kint), intent(in) :: i_vect, i_scalar, i_tensor
      real (kind = kreal), intent(in) :: ak_diff(ele1%numele)
      real (kind = kreal), intent(in) :: coef_field
!
      integer(kind=kint) :: k2, i, igrp, nd, num
!
!
      if (nmax_sf .eq. 0) return
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
! --------- set vector at each node in an element
!
      do nd = 1, n_vector
        do i = 1, ngrp_sf(nd)
          igrp = id_grp_sf(i,nd)
          num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
!
          if (num .gt. 0) then
            do k2 = 1, surf1%nnod_4_surf
              call d_SGS_flux_2_each_sf_w_cst(sf_grp, igrp, k2, nd,     &
     &            i_vect, i_scalar, i_tensor, dminus, vect_sf)
              call fem_sf_grp_skv_sgs_div_flux_p                        &
     &           (ele1, surf1, sf_grp, jac1_sf_grp_2d_q, FEM1_elen,     &
     &            igrp, k2, nd, n_int, i_filter, dxe_sf, vect_sf,       &
     &            ak_diff, coef_field, fem1_wk%sk6)
            end do
!
          end if
        end do
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_sf_skv_sgs_div_t_flux
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_skv_commute_sgs_v_flux(sf_grp, n_int, ngrp_sf,  &
     &          id_grp_sf, i_filter, i_tensor, i_vect, i_scalar)
!
      use m_geometry_data
      use m_sorted_node
      use m_finite_element_matrix
      use m_int_surface_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_filter_elength
!
      use delta_SGS_2_each_surface
      use fem_surf_skv_sgs_commute_t
      use cal_skv_to_ff_smp
!
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind=kint), intent(in) :: ngrp_sf
      integer(kind=kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer(kind=kint), intent(in) :: i_vect, i_scalar, i_tensor
!
      integer(kind=kint) :: k2, i, igrp, num
!
!
      if (ngrp_sf .eq. 0) return
      call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
! --------- set vector at each node in an element
!
      do i = 1, ngrp_sf
        igrp = id_grp_sf(i)
        num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
!
        if (num .gt. 0) then
          do k2 = 1, surf1%nnod_4_surf
            call d_SGS_flux_2_each_surface(sf_grp, igrp, k2, ione,      &
     &          i_vect, i_scalar, i_tensor, vect_sf)
            call fem_sf_grp_skv_div_f_commute_p                         &
     &         (ele1, surf1, sf_grp, jac1_sf_grp_2d_q, FEM1_elen,       &
     &          igrp, k2, ione, n_int, i_filter, dxe_sf, vect_sf,       &
     &          fem1_wk%sk6)
          end do
!
        end if
      end do
!
      call add1_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_sf_skv_commute_sgs_v_flux
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_skv_commute_sgs_t_flux                          &
     &         (sf_grp, n_int, nmax_sf, ngrp_sf,                        &
     &          id_grp_sf, i_filter, i_tensor, i_vect, i_scalar)
!
      use m_geometry_data
      use m_sorted_node
      use m_finite_element_matrix
      use m_int_surface_data
      use m_jacobians
      use m_jacobian_sf_grp
      use m_filter_elength
!
      use delta_SGS_2_each_surface
      use fem_surf_skv_sgs_commute_t
      use cal_skv_to_ff_smp
!
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind=kint), intent(in) :: nmax_sf
      integer(kind=kint), intent(in) :: ngrp_sf(3)
      integer(kind=kint), intent(in) :: id_grp_sf(nmax_sf,3)
      integer(kind=kint), intent(in) :: n_int, i_filter
      integer(kind=kint), intent(in) :: i_vect, i_scalar, i_tensor
!
      integer(kind=kint) :: k2, i, igrp, nd, num
!
!
      if (nmax_sf .eq. 0) return
      call reset_sk6(n_vector, ele1, fem1_wk%sk6)
!
! --------- set vector at each node in an element
!
      do nd = 1, n_vector
        do i = 1, ngrp_sf(nd)
          igrp = id_grp_sf(i,nd)
          num = sf_grp%istack_grp(igrp) - sf_grp%istack_grp(igrp-1)
!
          if (num .gt. 0) then
            do k2 = 1, surf1%nnod_4_surf
              call d_SGS_flux_2_each_surface(sf_grp, igrp, k2,          &
     &            nd, i_vect, i_scalar, i_tensor, vect_sf)
              call fem_sf_grp_skv_div_f_commute_p                       &
     &           (ele1, surf1, sf_grp, jac1_sf_grp_2d_q, FEM1_elen,     &
     &            igrp, k2, nd, n_int, i_filter, dxe_sf, vect_sf,       &
     &            fem1_wk%sk6)
            end do
!
          end if
        end do
      end do
!
      call add3_skv_to_ff_v_smp(node1, ele1, rhs_tbl1,                  &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_sf_skv_commute_sgs_t_flux
!
!-----------------------------------------------------------------------
!
      end module int_surf_div_fluxes_sgs
