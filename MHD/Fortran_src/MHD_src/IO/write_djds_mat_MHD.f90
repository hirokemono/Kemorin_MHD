!write_djds_mat_MHD.f90
!     module   write_djds_mat_MHD
!
!     Written by H. Matsui on Apr., 2008
!
!      subroutine s_write_djds_mat_MHD
!
!      subroutine write_djds_mat_velo
!      subroutine write_djds_mat_press
!      subroutine write_djds_mat_magne
!      subroutine write_djds_mat_mag_p
!      subroutine write_djds_mat_temp
!      subroutine write_djds_mat_composition
!
      module   write_djds_mat_MHD
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_iccg_parameter
!
      use m_type_AMG_data
      use set_parallel_file_name
!
      implicit  none
!
      integer(kind = kint), parameter :: id_mat_file = 14
      character(len=kchara) :: fhead_velo_mat =    'djds_mat_velo'
      character(len=kchara) :: fhead_press_mat =   'djds_mat_press'
      character(len=kchara) :: fhead_magne_mat =   'djds_mat_magne'
      character(len=kchara) :: fhead_magp_mat =    'djds_mat_magp'
      character(len=kchara) :: fhead_temp_mat =    'djds_mat_temp'
      character(len=kchara) :: fhead_dscalar_mat = 'djds_mat_scalar'
!
      character(len=kchara) :: fname, fname_tmp
!
      private ::  fname, fname_tmp, id_mat_file
      private :: write_MHD_djds_mat33, write_MHD_djds_mat11
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_write_djds_mat_MHD
!
      use m_control_parameter
      use m_solver_djds_MHD
      use m_type_AMG_data_4_MHD
!
!
      if ( iflag_t_evo_4_velo .gt. id_no_evolution) then
        call write_MHD_djds_mat11                                       &
     &      (fhead_press_mat, DJDS_comm_fl, DJDS_fl_l, Pmat_DJDS,       &
     &       num_MG_level, MG_comm_fl, MG_djds_tbl_fll, MG_mat_press)
      end if
!
      if ( iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        call write_MHD_djds_mat33                                       &
     &      (fhead_velo_mat, DJDS_comm_fl, DJDS_fluid,                  &
     &       MHD1_matrices%Vmat_MG_DJDS(0),                             &
     &       num_MG_level, MG_comm_fl, MG_djds_tbl_fl,                  &
     &       MHD1_matrices%Vmat_MG_DJDS)
      end if
!
      if ( iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        call write_MHD_djds_mat11                                       &
     &      (fhead_temp_mat, DJDS_comm_fl, DJDS_fluid, Tmat_DJDS,       &
     &       num_MG_level, MG_comm_fl, MG_djds_tbl_fl, MG_mat_temp)
      end if
!
      if ( iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        call write_MHD_djds_mat11                                       &
     &      (fhead_dscalar_mat, DJDS_comm_fl, DJDS_fluid, Cmat_DJDS,    &
     &       num_MG_level, MG_comm_fl, MG_djds_tbl_fl, MG_mat_d_scalar)
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution                     &
     &     .or. iflag_t_evo_4_magne .gt. id_no_evolution) then
        call write_MHD_djds_mat11                                       &
     &      (fhead_magp_mat, DJDS_comm_etr, DJDS_linear, Fmat_DJDS,     &
     &       num_MG_level, MG_comm, MG_djds_tbl_l, MG_mat_magp)
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution                     &
     &     .or. iflag_t_evo_4_magne .gt. id_no_evolution) then
        call write_MHD_djds_mat33                                       &
     &      (fhead_magne_mat, DJDS_comm_etr, DJDS_entire, Bmat_DJDS,    &
     &       num_MG_level, MG_comm, MG_djds_tbl_fl, MG_mat_magne)
      end if
!
      end subroutine s_write_djds_mat_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_MHD_djds_mat33                                   &
     &      (fhead_matrix, DJDS_comm, DJDS_tbl, mat33_DJDS,             &
     &       num_MG_level, MG_comm, MG_djds_tbl, MG_mat33)
!
      use write_djds_matrix_struct
      use skip_comment_f
!
      character(len=kchara) :: fhead_matrix
      type(communication_table), intent(in) :: DJDS_comm
      type(DJDS_ordering_table), intent(in) :: DJDS_tbl
      type(DJDS_MATRIX), intent(in) :: mat33_DJDS
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(communication_table), intent(in)                             &
     &                      :: MG_comm(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &                      :: MG_djds_tbl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: MG_mat33(0:num_MG_level)
!
      integer(kind = kint) :: i
!
!
      if (cmp_no_case(METHOD, 'MGCG')) then
        do i = 0, num_MG_level
          call add_int_suffix(i, fhead_matrix, fname_tmp)
          call add_int_suffix(my_rank, fname_tmp, fname)
          open(id_mat_file, file=fname)
          call write_djds_mat33_comp_type(id_mat_file,  np_smp,         &
     &        MG_djds_tbl(i), MG_mat33(i) )
          call write_djds_mat_connect_type(id_mat_file,  np_smp,        &
     &        MG_comm(i), MG_djds_tbl(i), MG_mat33(i) )
          close(id_mat_file)
        end do
      else
        call add_int_suffix(my_rank, fhead_matrix, fname)
        open(id_mat_file, file=fname)
        call write_djds_mat33_comp_type(id_mat_file, np_smp,            &
     &      DJDS_tbl, mat33_DJDS)
        call write_djds_mat_connect_type(id_mat_file, np_smp,           &
     &      DJDS_comm, DJDS_tbl, mat33_DJDS)
        close(id_mat_file)
      end if
!
      end subroutine write_MHD_djds_mat33
!
! ----------------------------------------------------------------------
!
      subroutine write_MHD_djds_mat11                                   &
     &      (fhead_matrix, DJDS_comm, DJDS_tbl, mat11_DJDS,             &
     &       num_MG_level, MG_comm, MG_djds_tbl, MG_mat11)
!
      use write_djds_matrix_struct
      use skip_comment_f
!
      character(len=kchara) :: fhead_matrix
      type(communication_table), intent(in) :: DJDS_comm
      type(DJDS_ordering_table), intent(in) :: DJDS_tbl
      type(DJDS_MATRIX), intent(in) :: mat11_DJDS
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(communication_table), intent(in)                             &
     &                      :: MG_comm(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &                      :: MG_djds_tbl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: MG_mat11(0:num_MG_level)
!
      integer(kind = kint) :: i
!
!
      if (cmp_no_case(METHOD, 'MGCG')) then
        do i = 0, num_MG_level
          call add_int_suffix(i, fhead_matrix, fname_tmp)
          call add_int_suffix(my_rank, fname_tmp, fname)
          open(id_mat_file, file=fname)
          call write_djds_mat11_comp_type(id_mat_file,  np_smp,         &
     &        MG_djds_tbl(i), MG_mat11(i) )
          call write_djds_mat_connect_type(id_mat_file,  np_smp,        &
     &        MG_comm(i), MG_djds_tbl(i), MG_mat11(i) )
          close(id_mat_file)
        end do
      else
        call add_int_suffix(my_rank, fhead_matrix, fname)
        open(id_mat_file, file=fname)
        call write_djds_mat11_comp_type(id_mat_file, np_smp,            &
     &      DJDS_tbl, mat11_DJDS)
        call write_djds_mat_connect_type(id_mat_file, np_smp,           &
     &      DJDS_comm, DJDS_tbl, mat11_DJDS)
        close(id_mat_file)
      end if
!
      end subroutine write_MHD_djds_mat11
!
! ----------------------------------------------------------------------
!
      end module write_djds_mat_MHD
