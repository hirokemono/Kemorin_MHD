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
      use m_machine_parameter
      use calypso_mpi
      use m_iccg_parameter
      use m_geometry_parameter
!
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
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
!
!
      if ( iflag_t_evo_4_velo .gt. id_no_evolution) then
        call write_djds_mat_press
      end if
!
      if ( iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        call write_djds_mat_velo
      end if
!
      if ( iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        call write_djds_mat_temp
      end if
!
      if ( iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        call write_djds_mat_composition
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution                     &
     &     .or. iflag_t_evo_4_magne .gt. id_no_evolution) then
        call write_djds_mat_mag_p
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution                     &
     &     .or. iflag_t_evo_4_magne .gt. id_no_evolution) then
        call write_djds_mat_magne
      end if
!
      end subroutine s_write_djds_mat_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_djds_mat_velo
!
      use m_solver_djds_MHD
      use m_velo_matrix
      use write_djds_matrix_struct
!
      integer(kind = kint) :: i
!
!
      if ( ((METHOD(1:1).eq.'M').or.(METHOD(1:1).eq.'m')) .and.         &
     &     ((METHOD(2:2).eq.'G').or.(METHOD(2:2).eq.'g')) .and.         &
     &     ((METHOD(3:3).eq.'C').or.(METHOD(3:3).eq.'c')) .and.         &
     &     ((METHOD(4:4).eq.'G').or.(METHOD(4:4).eq.'g')) ) then
        do i = 0, num_MG_level
          call add_int_suffix(i, fhead_velo_mat, fname_tmp)
          call add_int_suffix(my_rank, fname_tmp, fname)
          open(id_mat_file, file=fname)
          call write_djds_mat33_comp_type(id_mat_file,  np_smp,         &
     &        MG_djds_tbl_fl(i), MG_mat_velo(i) )
          call write_djds_mat_connect_type(id_mat_file,  np_smp,        &
     &        MG_comm_fl(i), MG_djds_tbl_fl(i), MG_mat_velo(i) )
          close(id_mat_file)
        end do
      else
        call add_int_suffix(my_rank, fhead_velo_mat, fname)
        open(id_mat_file, file=fname)
        call write_djds_mat33_comp_type(id_mat_file, np_smp,            &
     &      DJDS_fluid, Vmat_DJDS)
        call write_djds_mat_connect_type(id_mat_file, np_smp,           &
     &      DJDS_comm_fl, DJDS_fluid, Vmat_DJDS)
        close(id_mat_file)
      end if
!
      end subroutine write_djds_mat_velo
!
! ----------------------------------------------------------------------
!
      subroutine write_djds_mat_press
!
      use m_solver_djds_MHD
      use m_velo_matrix
      use write_djds_matrix_struct
!
      integer(kind = kint) :: i
!
!
      if ( ((METHOD(1:1).eq.'M').or.(METHOD(1:1).eq.'m')) .and.         &
     &     ((METHOD(2:2).eq.'G').or.(METHOD(2:2).eq.'g')) .and.         &
     &     ((METHOD(3:3).eq.'C').or.(METHOD(3:3).eq.'c')) .and.         &
     &     ((METHOD(4:4).eq.'G').or.(METHOD(4:4).eq.'g')) ) then
        do i = 0, num_MG_level
          call add_int_suffix(i, fhead_press_mat, fname_tmp)
          call add_int_suffix(my_rank, fname_tmp, fname)
          open(id_mat_file, file=fname)
          call write_djds_mat11_comp_type(id_mat_file,  np_smp,         &
     &        MG_djds_tbl_fll(i), MG_mat_press(i) )
          call write_djds_mat_connect_type(id_mat_file,  np_smp,        &
     &        MG_comm_fl(i), MG_djds_tbl_fll(i), MG_mat_press(i) )
          close(id_mat_file)
        end do
      else
        call add_int_suffix(my_rank, fhead_press_mat, fname)
        open(id_mat_file, file=fname)
        call write_djds_mat11_comp_type(id_mat_file, np_smp,            &
     &      DJDS_fl_l, Pmat_DJDS)
        call write_djds_mat_connect_type(id_mat_file, np_smp,           &
     &      DJDS_comm_fl, DJDS_fl_l, Pmat_DJDS)
        close(id_mat_file)
      end if
!
      end subroutine write_djds_mat_press
!
! ----------------------------------------------------------------------
!
      subroutine write_djds_mat_magne
!
      use m_solver_djds_MHD
      use m_magne_matrix
      use write_djds_matrix_struct
      use skip_comment_f
!
      integer(kind = kint) :: i
!
!
      if (cmp_no_case(METHOD, 'MGCG')) then
        do i = 0, num_MG_level
          call add_int_suffix(i, fhead_magne_mat, fname_tmp)
          call add_int_suffix(my_rank, fname_tmp, fname)
          open(id_mat_file, file=fname)
          call write_djds_mat33_comp_type(id_mat_file,  np_smp,         &
     &        MG_djds_tbl(i), MG_mat_magne(i) )
          call write_djds_mat_connect_type(id_mat_file,  np_smp,        &
     &        MG_comm(i), MG_djds_tbl_fl(i), MG_mat_magne(i) )
          close(id_mat_file)
        end do
      else
        call add_int_suffix(my_rank, fhead_magne_mat, fname)
        open(id_mat_file, file=fname)
        call write_djds_mat33_comp_type(id_mat_file, np_smp,            &
     &      DJDS_entire, Bmat_DJDS)
        call write_djds_mat_connect_type(id_mat_file, np_smp,           &
     &      DJDS_comm_etr, DJDS_entire, Bmat_DJDS)
        close(id_mat_file)
      end if
!
      end subroutine write_djds_mat_magne
!
! ----------------------------------------------------------------------
!
      subroutine write_djds_mat_mag_p
!
      use m_solver_djds_MHD
      use m_magne_matrix
      use write_djds_matrix_struct
      use skip_comment_f
!
      integer(kind = kint) :: i
!
!
      if (cmp_no_case(METHOD, 'MGCG')) then
        do i = 0, num_MG_level
          call add_int_suffix(i, fhead_magp_mat, fname_tmp)
          call add_int_suffix(my_rank, fname_tmp, fname)
          open(id_mat_file, file=fname)
          call write_djds_mat11_comp_type(id_mat_file,  np_smp,         &
     &        MG_djds_tbl_l(i), MG_mat_magp(i) )
          call write_djds_mat_connect_type(id_mat_file,  np_smp,        &
     &        MG_comm(i), MG_djds_tbl_l(i), MG_mat_magp(i) )
          close(id_mat_file)
        end do
      else
        call add_int_suffix(my_rank, fhead_magp_mat, fname)
        open(id_mat_file, file=fname)
        call write_djds_mat11_comp_type(id_mat_file, np_smp,            &
     &      DJDS_linear, Fmat_DJDS)
        call write_djds_mat_connect_type(id_mat_file, np_smp,           &
     &      DJDS_comm_etr, DJDS_linear, Fmat_DJDS)
        close(id_mat_file)
      end if
!
      end subroutine write_djds_mat_mag_p
!
! ----------------------------------------------------------------------
!
      subroutine write_djds_mat_temp
!
      use m_solver_djds_MHD
      use m_temp_matrix
      use write_djds_matrix_struct
      use skip_comment_f
!
      integer(kind = kint) :: i
!
!
      if (cmp_no_case(METHOD, 'MGCG')) then
        do i = 0, num_MG_level
          call add_int_suffix(i, fhead_temp_mat, fname_tmp)
          call add_int_suffix(my_rank, fname_tmp, fname)
          open(id_mat_file, file=fname)
          call write_djds_mat11_comp_type(id_mat_file,  np_smp,         &
     &        MG_djds_tbl_fl(i), MG_mat_temp(i) )
          call write_djds_mat_connect_type(id_mat_file,  np_smp,        &
     &        MG_comm_fl(i), MG_djds_tbl_fl(i), MG_mat_temp(i) )
          close(id_mat_file)
        end do
      else
        call add_int_suffix(my_rank, fhead_temp_mat, fname)
        open(id_mat_file, file=fname)
        call write_djds_mat11_comp_type(id_mat_file, np_smp,            &
     &      DJDS_fluid, Tmat_DJDS)
        call write_djds_mat_connect_type(id_mat_file, np_smp,           &
     &      DJDS_comm_fl, DJDS_fluid, Tmat_DJDS)
        close(id_mat_file)
      end if
!
      end subroutine write_djds_mat_temp
!
! ----------------------------------------------------------------------
!
      subroutine write_djds_mat_composition
!
      use m_solver_djds_MHD
      use m_light_element_matrix
      use write_djds_matrix_struct
      use skip_comment_f
!
      integer(kind = kint) :: i
!
!
      if (cmp_no_case(METHOD, 'MGCG')) then
        do i = 0, num_MG_level
          call add_int_suffix(i, fhead_dscalar_mat, fname_tmp)
          call add_int_suffix(my_rank, fname_tmp, fname)
          open(id_mat_file, file=fname)
          call write_djds_mat11_comp_type(id_mat_file,  np_smp,         &
     &        MG_djds_tbl_fl(i), MG_mat_d_scalar(i) )
          call write_djds_mat_connect_type(id_mat_file,  np_smp,        &
     &        MG_comm_fl(i), MG_djds_tbl_fl(i), MG_mat_d_scalar(i) )
          close(id_mat_file)
        end do
      else
        call add_int_suffix(my_rank, fhead_dscalar_mat, fname)
        open(id_mat_file, file=fname)
        call write_djds_mat11_comp_type(id_mat_file, np_smp,            &
     &      DJDS_fluid, Cmat_DJDS)
        call write_djds_mat_connect_type(id_mat_file, np_smp,           &
     &      DJDS_comm_fl, DJDS_fluid, Cmat_DJDS)
        close(id_mat_file)
      end if
!
      end subroutine write_djds_mat_composition
!
! ----------------------------------------------------------------------
!
      end module write_djds_mat_MHD
