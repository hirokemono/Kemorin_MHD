!
!     module cal_multi_pass
!
!     Written by H. Matsui on June, 2005
!
!      subroutine cal_t_evo_4_vector(iflag_4_supg)
!      subroutine cal_t_evo_4_scalar(iflag_4_supg)
!      subroutine cal_t_evo_4_vector_fl(iflag_4_supg)
!      subroutine cal_t_evo_4_scalar_fl(iflag_4_supg)
!      subroutine cal_t_evo_4_vector_cd(iflag_4_supg)
!      subroutine cal_t_evo_4_scalar_cd(iflag_4_supg)
!      subroutine cal_multi_pass_4_vector_ff
!      subroutine cal_multi_pass_4_scalar_ff
!      subroutine cal_multi_pass_4_vector_fl
!      subroutine cal_multi_pass_4_scalar_fl
!      subroutine cal_multi_pass_4_vector_cd
!      subroutine cal_multi_pass_4_scalar_cd
!      subroutine cal_multi_pass_4_vector_ins
!      subroutine cal_multi_pass_4_scalar_ins
!
      module cal_multi_pass
!
      use m_precision
!
      use m_control_parameter
      use m_geometry_data
      use m_phys_constants
      use m_sorted_node
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use int_multi_pass
      use int_multi_pass_upw
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_t_evo_4_vector(iflag_4_supg)
!
      integer(kind = kint), intent(in) :: iflag_4_supg
!
!
      if ( num_multi_pass .gt. 1 ) then
!
        call cal_ff_smp_2_multi_pass                                    &
     &     (node1%max_nod_smp, node1%istack_nod_smp,                    &
     &      n_vector, f1_nl%ff_smp, mhd_fem1_wk%ff_m_smp)
!
        if (iflag_4_supg .eq. id_turn_ON) then
          call int_multi_pass_vector_upw
        else if (iflag_4_supg .eq. id_magnetic_SUPG) then
          call int_multi_pass_vector_upm
        else
          call int_multi_pass_vector
        end if
!
      end if
!
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
!
      end subroutine cal_t_evo_4_vector
!
! ----------------------------------------------------------------------
!
      subroutine cal_t_evo_4_scalar(iflag_4_supg)
!
      integer(kind = kint), intent(in) :: iflag_4_supg
!
!
      if ( num_multi_pass .gt. 1 ) then
!
        call cal_ff_smp_2_multi_pass                                    &
     &     (node1%max_nod_smp, node1%istack_nod_smp,                    &
     &      n_scalar, f1_nl%ff_smp, mhd_fem1_wk%ff_m_smp)
!
        if (iflag_4_supg .eq. id_turn_ON) then
          call int_multi_pass_scalar_upw
        else if (iflag_4_supg .eq. id_magnetic_SUPG) then
          call int_multi_pass_scalar_upm
        else
          call int_multi_pass_scalar
        end if
!
      end if
!
      call set_ff_nl_smp_2_ff(n_scalar, node1, rhs_tbl1, f1_l, f1_nl)
!
      end subroutine cal_t_evo_4_scalar
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_t_evo_4_vector_fl(iflag_4_supg)
!
      integer(kind = kint), intent(in) :: iflag_4_supg
!
!
      if ( num_multi_pass .gt. 1 ) then
!
        call cal_ff_smp_2_multi_pass                                    &
     &     (node1%max_nod_smp, node1%istack_nod_smp,                    &
     &      n_vector, f1_nl%ff_smp, mhd_fem1_wk%ff_m_smp)
!
        if (iflag_4_supg .eq. id_turn_ON) then
          call int_multi_pass_vector_fl_upw
        else if (iflag_4_supg .eq. id_magnetic_SUPG) then
          call int_multi_pass_vector_fl_upm
        else
          call int_multi_pass_vector_fl
        end if
!
      end if
!
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
!
      end subroutine cal_t_evo_4_vector_fl
!
! ----------------------------------------------------------------------
!
      subroutine cal_t_evo_4_scalar_fl(iflag_4_supg)
!
      integer(kind = kint), intent(in) :: iflag_4_supg
!
!
      if ( num_multi_pass .gt. 1 ) then
!
        call cal_ff_smp_2_multi_pass                                    &
     &     (node1%max_nod_smp, node1%istack_nod_smp,                    &
     &      n_scalar, f1_nl%ff_smp, mhd_fem1_wk%ff_m_smp)
!
        if (iflag_4_supg .eq. id_turn_ON) then
          call int_multi_pass_scalar_fl_upw
        else if (iflag_4_supg .eq. id_magnetic_SUPG) then
          call int_multi_pass_scalar_fl_upm
        else
          call int_multi_pass_scalar_fl
        end if
!
      end if
!
      call set_ff_nl_smp_2_ff(n_scalar, node1, rhs_tbl1, f1_l, f1_nl)
!
      end subroutine cal_t_evo_4_scalar_fl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_t_evo_4_vector_cd(iflag_4_supg)
!
      integer(kind = kint), intent(in) :: iflag_4_supg
!
!
      if ( num_multi_pass .gt. 1 ) then
!
        call cal_ff_smp_2_multi_pass                                    &
     &     (node1%max_nod_smp, node1%istack_nod_smp,                    &
     &      n_vector, f1_nl%ff_smp, mhd_fem1_wk%ff_m_smp)
!
        if (iflag_4_supg .gt. id_turn_OFF) then
          call int_multi_pass_vector_cd_upm
        else
          call int_multi_pass_vector_cd
        end if
!
      end if
!
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
!
      end subroutine cal_t_evo_4_vector_cd
!
! ----------------------------------------------------------------------
!
      subroutine cal_t_evo_4_scalar_cd(iflag_4_supg)
!
      integer(kind = kint), intent(in) :: iflag_4_supg
!
!
      if ( num_multi_pass .gt. 1 ) then
!
        call cal_ff_smp_2_multi_pass                                    &
     &     (node1%max_nod_smp, node1%istack_nod_smp,                    &
     &      n_scalar, f1_nl%ff_smp, mhd_fem1_wk%ff_m_smp)
!
        if (iflag_4_supg .gt. id_turn_OFF) then
          call int_multi_pass_scalar_cd_upm
        else
          call int_multi_pass_scalar_cd
        end if
!
      end if
!
      call set_ff_nl_smp_2_ff(n_scalar, node1, rhs_tbl1, f1_l, f1_nl)
!
      end subroutine cal_t_evo_4_scalar_cd
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_multi_pass_4_vector_ff
!
!
      if ( num_multi_pass .gt. 1 ) then
        call cal_ff_smp_2_multi_pass                                    &
     &     (node1%max_nod_smp, node1%istack_nod_smp,                    &
     &      n_vector, f1_nl%ff_smp, mhd_fem1_wk%ff_m_smp)
        call int_multi_pass_vector
      end if
!
      call reset_ff(node1%numnod, f1_l)
      call cal_ff_smp_2_ff(node1, rhs_tbl1, n_vector,                   &
     &    f1_nl%ff_smp, f1_l%ff)
!
      end subroutine cal_multi_pass_4_vector_ff
!
! ----------------------------------------------------------------------
!
      subroutine cal_multi_pass_4_scalar_ff
!
!
      if ( num_multi_pass .gt. 1 ) then
        call cal_ff_smp_2_multi_pass                                    &
     &     (node1%max_nod_smp, node1%istack_nod_smp,                    &
     &      n_scalar, f1_nl%ff_smp, mhd_fem1_wk%ff_m_smp)
        call int_multi_pass_scalar
      end if
!
      call reset_ff(node1%numnod, f1_l)
      call cal_ff_smp_2_ff(node1, rhs_tbl1, n_scalar,                   &
     &    f1_nl%ff_smp, f1_l%ff)
!
      end subroutine cal_multi_pass_4_scalar_ff
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_multi_pass_4_vector_fl
!
!
      if ( num_multi_pass .gt. 1 ) then
        call cal_ff_smp_2_multi_pass                                    &
     &     (node1%max_nod_smp, node1%istack_nod_smp,                    &
     &      n_vector, f1_nl%ff_smp, mhd_fem1_wk%ff_m_smp)
        call int_multi_pass_vector_fl
      end if
!
      call reset_ff(node1%numnod, f1_l)
      call cal_ff_smp_2_ff(node1, rhs_tbl1, n_vector,                   &
     &    f1_nl%ff_smp, f1_l%ff)
!
      end subroutine cal_multi_pass_4_vector_fl
!
! ----------------------------------------------------------------------
!
      subroutine cal_multi_pass_4_scalar_fl
!
!
      if ( num_multi_pass .gt. 1 ) then
        call cal_ff_smp_2_multi_pass                                    &
     &     (node1%max_nod_smp, node1%istack_nod_smp,                    &
     &      n_scalar, f1_nl%ff_smp, mhd_fem1_wk%ff_m_smp)
        call int_multi_pass_scalar_fl
      end if
!
      call reset_ff(node1%numnod, f1_l)
      call cal_ff_smp_2_ff(node1, rhs_tbl1, n_scalar,                   &
     &    f1_nl%ff_smp, f1_l%ff)
!
      end subroutine cal_multi_pass_4_scalar_fl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_multi_pass_4_vector_cd
!
!
      if ( num_multi_pass .gt. 1 ) then
        call cal_ff_smp_2_multi_pass                                    &
     &     (node1%max_nod_smp, node1%istack_nod_smp,                    &
     &      n_vector, f1_nl%ff_smp, mhd_fem1_wk%ff_m_smp)
        call int_multi_pass_vector_cd
      end if
!
      call reset_ff(node1%numnod, f1_l)
      call cal_ff_smp_2_ff(node1, rhs_tbl1, n_vector,                   &
     &    f1_nl%ff_smp, f1_l%ff)
!
      end subroutine cal_multi_pass_4_vector_cd
!
! ----------------------------------------------------------------------
!
      subroutine cal_multi_pass_4_scalar_cd
!
!
      if ( num_multi_pass .gt. 1 ) then
        call cal_ff_smp_2_multi_pass                                    &
     &     (node1%max_nod_smp, node1%istack_nod_smp,                    &
     &      n_scalar, f1_nl%ff_smp, mhd_fem1_wk%ff_m_smp)
        call int_multi_pass_scalar_cd
      end if
!
      call reset_ff(node1%numnod, f1_l)
      call cal_ff_smp_2_ff(node1, rhs_tbl1, n_scalar,                   &
     &    f1_nl%ff_smp, f1_l%ff)
!
      end subroutine cal_multi_pass_4_scalar_cd
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_multi_pass_4_vector_ins
!
!
      if ( num_multi_pass .gt. 1 ) then
        call cal_ff_smp_2_multi_pass                                    &
     &     (node1%max_nod_smp, node1%istack_nod_smp,                    &
     &      n_vector, f1_nl%ff_smp, mhd_fem1_wk%ff_m_smp)
        call int_multi_pass_vector_ins
      end if
!
      call reset_ff(node1%numnod, f1_l)
      call cal_ff_smp_2_ff(node1, rhs_tbl1, n_vector,                   &
     &    f1_nl%ff_smp, f1_l%ff)
!
      end subroutine cal_multi_pass_4_vector_ins
!
! ----------------------------------------------------------------------
!
      subroutine cal_multi_pass_4_scalar_ins
!
!
      if ( num_multi_pass .gt. 1 ) then
        call cal_ff_smp_2_multi_pass                                    &
     &     (node1%max_nod_smp, node1%istack_nod_smp,                    &
     &      n_scalar, f1_nl%ff_smp, mhd_fem1_wk%ff_m_smp)
        call int_multi_pass_scalar_ins
      end if
!
      call reset_ff(node1%numnod, f1_l)
      call cal_ff_smp_2_ff(node1, rhs_tbl1, n_scalar,                   &
     &    f1_nl%ff_smp, f1_l%ff)
!
      end subroutine cal_multi_pass_4_scalar_ins
!
! ----------------------------------------------------------------------
!
      end module cal_multi_pass
