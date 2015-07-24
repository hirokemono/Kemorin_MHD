!int_surf_fixed_gradients.f90
!      module int_surf_fixed_gradients
!
!      Written by H. Matsui on Sep., 2005
!
!      subroutine int_sf_h_flux(n_int)
!      subroutine int_sf_torque(n_int)
!      subroutine int_sf_grad_vecp(n_int)
!      subroutine int_sf_grad_magne(n_int)
!      subroutine int_sf_grad_composition(n_int)
!
      module int_surf_fixed_gradients
!
      use m_precision
!
      use m_constants
      use m_control_parameter
      use m_ele_material_property
      use m_finite_element_matrix
      use m_phys_constants
      use m_surface_group
!
      use fem_surf_skv_poisson_1st
      use cal_skv_to_ff_smp_1st
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_h_flux(n_int)
!
      use m_surf_data_temp
!
      integer(kind=kint), intent(in) :: n_int
!
!
      if (ngrp_sf_fix_hf .le. 0) return
      call reset_sk6(n_scalar)
!
      call fem_surf_skv_norm_grad_1(sf_grp1, ngrp_sf_fix_hf,            &
     &    nele_sf_fix_hf, ngrp_sf_fix_hf,                               &
     &    id_grp_sf_fix_hf, ist_ele_sf_fix_hf,                          &
     &    sf_apt_fix_hf, n_int, ione, ak_d_temp, sk6)
!
      call add1_skv_to_ff_v_smp_1st(ff_smp, sk6)
!
      end subroutine int_sf_h_flux
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_torque(n_int)
!
      use m_surf_data_torque
!
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint) :: nd
!
!
      if ( sum(ngrp_sf_fix_tq) .le. 0) return
      call reset_sk6(n_vector)
!
      do nd = 1, n_vector
       if (ngrp_sf_fix_tq(nd).gt.0) then
         call fem_surf_skv_norm_grad_1(sf_grp1, nmax_sf_fix_tq,         &
     &       nmax_ele_sf_fix_tq, ngrp_sf_fix_tq(nd),                    &
     &       id_grp_sf_fix_tq(1,nd), ist_ele_sf_fix_tq(0,nd),           &
     &       sf_apt_fix_tq(1,nd), n_int, nd, ak_d_velo, sk6)
       end if
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_smp, sk6)
!
      end subroutine int_sf_torque
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_grad_vecp(n_int)
!
     use m_surf_data_vector_p
!
      integer(kind=kint), intent(in) :: n_int
      integer(kind = kint) :: nd
!
!
      if ( sum(ngrp_sf_fix_grad_a) .le. 0) return
      call reset_sk6(n_vector)
!
      do nd = 1, n_vector
       if (ngrp_sf_fix_grad_a(nd).gt.0) then
         call fem_surf_skv_norm_grad_1(sf_grp1, nmax_sf_fix_grad_a,     &
     &       nmax_ele_sf_fix_grad_a, ngrp_sf_fix_grad_a(nd),            &
     &       id_grp_sf_fix_grad_a(1,nd), ist_ele_sf_fix_grad_a(0,nd),   &
     &       sf_apt_fix_grad_a(1,nd), n_int, nd, ak_d_magne, sk6)
       end if
      end do
      call add3_skv_to_ff_v_smp_1st(ff_smp, sk6)
!
      end subroutine int_sf_grad_vecp
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_grad_magne(n_int)
!
     use m_surf_data_magne
!
      integer(kind=kint), intent(in) :: n_int
!
      integer(kind = kint) :: nd
!
!
      if ( sum(ngrp_sf_fix_grad_b) .le. 0) return
      call reset_sk6(n_vector)
!
      do nd = 1, n_vector
        if (ngrp_sf_fix_grad_b(nd).gt.0) then
          call fem_surf_skv_norm_grad_1(sf_grp1, nmax_sf_fix_grad_b,    &
     &       nmax_ele_sf_fix_grad_b, ngrp_sf_fix_grad_b(nd),            &
     &       id_grp_sf_fix_grad_b(1,nd), ist_ele_sf_fix_grad_b(0,nd),   &
     &       sf_apt_fix_grad_b(1,nd), n_int, nd, ak_d_magne, sk6)
        end if
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_smp, sk6)
!
      end subroutine int_sf_grad_magne
!
!-----------------------------------------------------------------------
!
      subroutine int_sf_grad_composition(n_int)
!
      use m_surf_data_composition
!
      integer(kind=kint), intent(in) :: n_int
!
!
      if (ngrp_sf_fix_cmg .le. 0) return
!
      call reset_sk6(n_scalar)
!
      call fem_surf_skv_norm_grad_1(sf_grp1, ngrp_sf_fix_cmg,           &
     &      nele_sf_fix_cmg, ngrp_sf_fix_cmg, id_grp_sf_fix_cmg,        &
     &      ist_ele_sf_fix_cmg, sf_apt_fix_cmg, n_int, ione,            &
     &      ak_d_composit, sk6)
!
      call add1_skv_to_ff_v_smp_1st(ff_smp, sk6)
!
      end subroutine int_sf_grad_composition
!
!-----------------------------------------------------------------------
!
      end module int_surf_fixed_gradients
