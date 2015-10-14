!
!      module int_MHD_mass_matrices
!
!   Lumped mass matrix for each area
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     Modified by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Oct. 2006
!
!      subroutine int_RHS_mass_matrices
!
      module int_MHD_mass_matrices
!
      use m_precision
!
      use m_geometry_constants
      use m_geometry_data
      use m_finite_element_matrix
!
      use int_vol_mass_matrix
      use check_finite_element_mat
!
      implicit none
!
      private :: int_mass_matrix_trilinear, int_mass_matrices_quad
      private :: int_mass_matrix_fluid, int_mass_matrix_fl_quad
      private :: int_mass_matrix_conduct, int_mass_matrix_cd_quad
      private :: int_mass_matrix_insulate, int_mass_matrix_ins_quad
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_RHS_mass_matrices
!
      use m_control_parameter
!
      integer(kind = kint) :: num_int
!
!
      num_int = intg_point_t_evo
!
      if     (ele1%nnod_4_ele.eq.num_t_quad                             &
     &   .or. ele1%nnod_4_ele.eq.num_t_lag) then
        call int_mass_matrices_quad(num_int)
      else
        call int_mass_matrix_trilinear(num_int)
      end if
!
      end subroutine int_RHS_mass_matrices
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_trilinear(n_int)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: n_int
!
!
      if (iflag_debug.eq.1)                                            &
     &  write(*,*) 'int_lump_mass_matrix_linear'
      call int_lump_mass_matrix_linear(n_int)
!
      if (iflag_debug.eq.1)                                            &
     &  write(*,*) 'int_mass_matrix_fluid'
      call int_mass_matrix_fluid(n_int)
!
      if (iflag_debug.eq.1)                                            &
     &  write(*,*) 'int_mass_matrix_conduct'
      call int_mass_matrix_conduct(n_int)
!
      if (iflag_debug.eq.1)                                            &
     &  write(*,*) 'int_mass_matrix_insulate'
      call int_mass_matrix_insulate(n_int)
!
      end subroutine int_mass_matrix_trilinear
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrices_quad(n_int)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: n_int
!
!
      if (iflag_debug.eq.1)                                            &
     &  write(*,*) 'int_lump_mass_matrix_quad'
      call int_lump_mass_matrix_quad(n_int)
!
      if (iflag_debug.eq.1)                                            &
     &  write(*,*) 'int_mass_matrix_fl_quad'
      call int_mass_matrix_fl_quad(n_int)
!
      if (iflag_debug.eq.1)                                            &
     &  write(*,*) 'int_mass_matrix_cd_quad'
      call int_mass_matrix_cd_quad(n_int)
!
      if (iflag_debug.eq.1)                                            &
     &  write(*,*) 'int_mass_matrix_ins_quad'
      call int_mass_matrix_ins_quad(n_int)
!
      end subroutine int_mass_matrices_quad
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_fluid(n_int)
!
      use cal_ff_smp_to_ffs
      use m_geometry_data_MHD
      use m_int_vol_data
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_mass_matrix_diag(iele_fl_smp_stack, n_int)
      call cal_ff_smp_2_ml(node1, rhs_tbl1,                             &
     &    mhd_fem1_wk%ml_fl, mhd_fem1_wk%ml_o_fl, f1_l%ff_smp)
!
!      call check_mass_martix_fluid(my_rank, node1%numnod, mhd_fem1_wk)
!
      end subroutine int_mass_matrix_fluid
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_conduct(n_int)
!
      use cal_ff_smp_to_ffs
      use m_geometry_data_MHD
      use m_int_vol_data
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_mass_matrix_diag(iele_cd_smp_stack, n_int)
      call cal_ff_smp_2_ml(node1, rhs_tbl1,                             &
     &    mhd_fem1_wk%ml_cd, mhd_fem1_wk%ml_o_cd, f1_l%ff_smp)
!
!      call check_mass_martix_conduct                                   &
!     &   (my_rank, node1%numnod, mhd_fem1_wk)
!
      end subroutine int_mass_matrix_conduct
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_insulate(n_int)
!
      use cal_ff_smp_to_ffs
      use m_geometry_data_MHD
      use m_int_vol_data
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_mass_matrix_diag(iele_ins_smp_stack, n_int)
      call cal_ff_smp_2_ml(node1, rhs_tbl1,                             &
     &    mhd_fem1_wk%ml_ins, mhd_fem1_wk%ml_o_ins, f1_l%ff_smp)
!
      end subroutine int_mass_matrix_insulate
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_fl_quad(n_int)
!
      use cal_ff_smp_to_ffs
      use m_geometry_data_MHD
      use m_int_vol_data
!
      integer(kind = kint), intent(in) :: n_int
!
!
       call int_mass_matrix_HRZ(iele_fl_smp_stack, n_int)
       call cal_ff_smp_2_ml(node1, rhs_tbl1,                            &
     &     mhd_fem1_wk%ml_fl, mhd_fem1_wk%ml_o_fl, f1_l%ff_smp)
!
!      call check_mass_martix_fluid(my_rank, node1%numnod, mhd_fem1_wk)
!
      end subroutine int_mass_matrix_fl_quad
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_cd_quad(n_int)
!
      use cal_ff_smp_to_ffs
      use m_geometry_data_MHD
      use m_int_vol_data
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_mass_matrix_HRZ(iele_cd_smp_stack, n_int)
      call cal_ff_smp_2_ml(node1, rhs_tbl1,                             &
     &    mhd_fem1_wk%ml_cd, mhd_fem1_wk%ml_o_cd, f1_l%ff_smp)
!
!      call check_mass_martix_conduct                                   &
!     &   (my_rank, node1%numnod, mhd_fem1_wk)
!
      end subroutine int_mass_matrix_cd_quad
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_ins_quad(n_int)
!
      use cal_ff_smp_to_ffs
      use m_geometry_data_MHD
      use m_int_vol_data
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call int_mass_matrix_HRZ(iele_ins_smp_stack, n_int)
      call cal_ff_smp_2_ml(node1, rhs_tbl1,                             &
     &    mhd_fem1_wk%ml_ins, mhd_fem1_wk%ml_o_ins, f1_l%ff_smp)
!
      end subroutine int_mass_matrix_ins_quad
!
!-----------------------------------------------------------------------
!
      end module int_MHD_mass_matrices
