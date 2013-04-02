!
!     module int_vol_poisson_matrix
!
! numerical integration for finite elememt equations(Poisson's equation)
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2003 (ver 1.1)
!        modifired by H. Matsui on June, 2005
!        modifired by H. Matsui on Nov., 2007
!
!      subroutine int_vol_poisson_matrices
!      subroutine int_vol_crank_matrices
!
      module int_vol_poisson_matrix
!
      use m_precision
!
      use m_control_parameter
!
      implicit none
!
      private :: sel_int_press_poisson_mat_sgs
      private :: sel_int_mag_p_poisson_mat_sgs
!
      private :: sel_int_velo_crank_mat_sgs
      private :: sel_int_magne_crank_mat_sgs
      private :: sel_int_vecp_crank_mat_sgs
      private :: sel_int_temp_crank_mat_sgs
      private :: sel_int_composit_crank_mat_sgs
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_poisson_matrices
!
!
      if (iflag_t_evo_4_velo .gt. 0) then
        call sel_int_press_poisson_mat_sgs(intg_point_poisson)
      end if
!
      if (     iflag_t_evo_4_magne .gt. 0                               &
     &    .or. iflag_t_evo_4_vect_p .gt. 0) then
        call sel_int_mag_p_poisson_mat_sgs(intg_point_poisson)
      end if
!
      end subroutine int_vol_poisson_matrices
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_crank_matrices
!
!
      if ( iflag_t_evo_4_velo .ge. 3 ) then
        call sel_int_velo_crank_mat_sgs(intg_point_t_evo)
      end if
!
      if ( iflag_t_evo_4_magne .ge. 3 ) then
        call sel_int_magne_crank_mat_sgs(intg_point_t_evo)
      end if
!
      if ( iflag_t_evo_4_vect_p .ge. 3 ) then
        call sel_int_vecp_crank_mat_sgs(intg_point_t_evo)
      end if
!
      if ( iflag_t_evo_4_temp .ge. 3 ) then
        call sel_int_temp_crank_mat_sgs(intg_point_t_evo)
      end if
!
      if ( iflag_t_evo_4_composit .ge. 3 ) then
        call sel_int_composit_crank_mat_sgs(intg_point_t_evo)
      end if
!
      end subroutine int_vol_crank_matrices
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_int_press_poisson_mat_sgs(n_int)
!
      use m_SGS_address
!
      use int_vol_poisson_phys_mat
      use int_vol_poisson_sgs_mat
!
      integer(kind = kint), intent(in) :: n_int
!
!
      if (iflag_commute_velo .eq. id_SGS_commute_ON) then
        call int_vol_velo_sgs_poisson_mat(n_int)
      else
        call int_vol_velo_poisson_mat(n_int)
      end if
!
      end subroutine sel_int_press_poisson_mat_sgs
!
! ----------------------------------------------------------------------
!
      subroutine sel_int_mag_p_poisson_mat_sgs(n_int)
!
      use m_SGS_address
!
      use int_vol_poisson_phys_mat
      use int_vol_poisson_sgs_mat
!
      integer(kind = kint), intent(in) :: n_int
!
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON) then
        call int_vol_magne_sgs_poisson_mat(n_int)
      else
        call int_vol_magne_poisson_mat(n_int)
      end if
!
      end subroutine sel_int_mag_p_poisson_mat_sgs
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_int_velo_crank_mat_sgs(n_int)
!
      use m_SGS_address
!
      use int_vol_poisson_phys_mat
      use int_vol_poisson_sgs_mat
!
      integer(kind = kint), intent(in) :: n_int
!
!
      if(iak_diff_v.gt.0) then
        call int_vol_velo_sgs_crank_mat(n_int)
      else
        call int_vol_velo_crank_mat(n_int)
      end if
!
      end subroutine sel_int_velo_crank_mat_sgs
!
! ----------------------------------------------------------------------
!
      subroutine sel_int_magne_crank_mat_sgs(n_int)
!
      use m_SGS_address
!
      use int_vol_poisson_phys_mat
      use int_vol_poisson_sgs_mat
!
      integer(kind = kint), intent(in) :: n_int
!
!
      if (iak_diff_b .gt. 0) then
        call int_vol_magne_sgs_crank_mat(n_int)
      else
        call int_vol_magne_crank_mat(n_int)
      end if
!
      end subroutine sel_int_magne_crank_mat_sgs
!
! ----------------------------------------------------------------------
!
      subroutine sel_int_vecp_crank_mat_sgs(n_int)
!
      use m_SGS_address
!
      use int_vol_poisson_phys_mat
      use int_vol_poisson_sgs_mat
!
      integer(kind = kint), intent(in) :: n_int
!
!
      if(iak_diff_b .gt. 0) then
        call int_vol_vecp_sgs_crank_mat(n_int)
      else
        call int_vol_vecp_crank_mat(n_int)
      end if
!
      end subroutine sel_int_vecp_crank_mat_sgs
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_int_temp_crank_mat_sgs(n_int)
!
      use m_SGS_address
!
      use int_vol_poisson_phys_mat
      use int_vol_poisson_sgs_mat
!
      integer(kind = kint), intent(in) :: n_int
!
!
      if(iak_diff_t .gt. 0) then
        call int_vol_temp_sgs_crank_mat(n_int)
      else
        call int_vol_temp_crank_mat(n_int)
      end if
!
      end subroutine sel_int_temp_crank_mat_sgs
!
! ----------------------------------------------------------------------
!
      subroutine sel_int_composit_crank_mat_sgs(n_int)
!
      use m_SGS_address
!
      use int_vol_poisson_phys_mat
      use int_vol_poisson_sgs_mat
!
      integer(kind = kint), intent(in) :: n_int
!
!
      if(iak_diff_c .gt. 0) then
        call int_vol_composit_sgs_crank_mat(n_int)
      else
        call int_vol_composit_crank_mat(n_int)
      end if
!
      end subroutine sel_int_composit_crank_mat_sgs
!
! ----------------------------------------------------------------------
!
      end module int_vol_poisson_matrix
