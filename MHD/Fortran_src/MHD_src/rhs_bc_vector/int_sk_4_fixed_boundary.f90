!
!     module int_sk_4_fixed_boundary
!
!      Written by H. Matsui on Oct., 2005
!
!       subroutine int_vol_sk_po_bc
!       subroutine int_vol_sk_mp_bc
!       subroutine int_vol_sk_mag_p_ins_bc
!
!       subroutine int_sk_4_fixed_temp
!       subroutine int_sk_4_fixed_part_temp
!       subroutine int_sk_4_fixed_velo
!       subroutine int_sk_4_fixed_vector_p
!       subroutine int_sk_4_fixed_magne
!       subroutine int_sk_4_fixed_composition
!
      module int_sk_4_fixed_boundary
!
      use m_precision
!
      use m_phys_constants
      use m_control_parameter
!
      use int_vol_fixed_phys_ele
      use int_vol_fixed_phys_sgs_ele
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_sk_po_bc
!
      use cal_ff_smp_to_ffs
!
!
      if (iflag_commute_velo .eq. id_SGS_commute_ON) then
        call int_vol_sf_press_sgs_ele
      else
        call int_vol_sf_press_ele
      end if
!
      call cal_ff_smp_2_ff(n_scalar, ff_smp, ff)
!
      end subroutine int_vol_sk_po_bc
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_sk_mp_bc
!
      use cal_ff_smp_to_ffs
!      use check_finite_element_mat
!
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON) then
        call int_vol_sf_mag_p_sgs_ele
      else
        call int_vol_sf_mag_p_ele
      end if
!
      call cal_ff_smp_2_ff(n_scalar, ff_smp, ff)
!      call check_ff(n_scalar)
!
      end subroutine int_vol_sk_mp_bc
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_sk_mag_p_ins_bc
!
      use cal_ff_smp_to_ffs
!
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON) then
        call int_vol_sf_mag_p_ins_sgs_ele
      else
        call int_vol_sf_mag_p_ins_ele
      end if
!
      call cal_ff_smp_2_ff(n_scalar,ff_smp,ff)
!
      end subroutine int_vol_sk_mag_p_ins_bc
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine int_sk_4_fixed_temp
!
!
       if (iflag_commute_temp .eq. id_SGS_commute_ON) then
         call int_vol_sf_temp_sgs_ele
       else
         call int_vol_sf_temp_ele
       end if
!
       end subroutine int_sk_4_fixed_temp
!
! ----------------------------------------------------------------------
!
       subroutine int_sk_4_fixed_part_temp
!
!
       if (iflag_commute_temp .eq. id_SGS_commute_ON) then
         call int_vol_sf_part_temp_sgs_ele
       else
         call int_vol_sf_part_temp_ele
       end if
!
       end subroutine int_sk_4_fixed_part_temp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
       subroutine int_sk_4_fixed_velo
!
!
       if (iflag_commute_velo .eq. id_SGS_commute_ON) then
         call int_vol_sf_velo_sgs_ele
       else
         call int_vol_sf_velo_ele
       end if
!
       end subroutine int_sk_4_fixed_velo
!
! ----------------------------------------------------------------------
!
       subroutine int_sk_4_fixed_vector_p
!
!
       if (iflag_commute_magne .eq. id_SGS_commute_ON) then
         call int_vol_sf_vec_p_sgs_ele
       else
         call int_vol_sf_vec_p_ele
       end if
!
       end subroutine int_sk_4_fixed_vector_p
!
! ----------------------------------------------------------------------
!
       subroutine int_sk_4_fixed_magne
!
!
       if (iflag_commute_magne .eq. id_SGS_commute_ON) then
         call int_vol_sf_magne_sgs_ele
       else
         call int_vol_sf_magne_ele
       end if
!
       end subroutine int_sk_4_fixed_magne
!
! ----------------------------------------------------------------------
!
       subroutine int_sk_4_fixed_composition
!
!
       if (iflag_commute_composit .eq. id_SGS_commute_ON) then
         call int_vol_sf_composition_sgs_ele
       else
         call int_vol_sf_composition_ele
       end if
!
       end subroutine int_sk_4_fixed_composition
!
! ----------------------------------------------------------------------
!
      end module int_sk_4_fixed_boundary
