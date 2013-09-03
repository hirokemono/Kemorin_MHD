!
!      module int_surf_magne_pre
!
!     Written by H. Matsui on June, 2005
!
!      subroutine int_surf_magne_pre_ele
!      subroutine int_surf_magne_monitor(i_field)
!
      module int_surf_magne_pre
!
      use m_precision
!
      use m_control_parameter
!
      use int_surf_div_induct_tsr_sgs
      use int_surf_fixed_gradients
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_magne_pre_ele
!
      use m_node_phys_address
!
      integer(kind = kint)  :: num_int
!
!
      num_int = intg_point_t_evo
!
      call int_sf_grad_magne(num_int)
!
       if (iflag_SGS_induction .ne. id_SGS_none                         &
     &     .and. iflag_commute_induction .eq. id_SGS_commute_ON) then
         call int_surf_div_induct_t_sgs(num_int, ifilter_final,         &
     &      iphys%i_SGS_induct_t, iphys%i_velo, iphys%i_magne)
      end if
!
      end subroutine int_surf_magne_pre_ele
!
! ----------------------------------------------------------------------
!
      subroutine int_surf_magne_monitor(i_field)
!
      use m_node_phys_address
!
      integer(kind= kint), intent(in) :: i_field
      integer(kind = kint)  :: num_int
!
!
      num_int = intg_point_t_evo
!
      if (i_field .eq. iphys%i_b_diffuse) then
        if (nmax_sf_fix_grad_b.gt.0) then
          call int_sf_grad_magne(num_int)
        end if
      end if
!
      if (i_field .eq. iphys%i_SGS_induction) then
        if (iflag_SGS_induction .ne. id_SGS_none                        &
     &     .and. iflag_commute_induction .eq. id_SGS_commute_ON) then
          call int_surf_div_induct_t_sgs(num_int, ifilter_final,        &
     &      iphys%i_SGS_induct_t, iphys%i_velo, iphys%i_magne)
        end if
      end if
!
      end subroutine int_surf_magne_monitor
!
! ----------------------------------------------------------------------
!
      end module int_surf_magne_pre
