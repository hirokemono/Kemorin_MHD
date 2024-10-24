!write_djds_mat_MHD.f90
!     module   write_djds_mat_MHD
!
!     Written by H. Matsui on Apr., 2008
!
!!      subroutine s_write_djds_mat_MHD(FEM_prm, MHD_prop, solver_pack)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(MHD_matrices_pack), intent(in) :: solver_pack
!!
!!      subroutine write_djds_mat_velo
!!      subroutine write_djds_mat_press
!!      subroutine write_djds_mat_magne
!!      subroutine write_djds_mat_mag_p
!!      subroutine write_djds_mat_temp
!!      subroutine write_djds_mat_composition
!
      module   write_djds_mat_MHD
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
!
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
      private :: fname, fname_tmp, id_mat_file
      private :: write_MHD_djds_mat33, write_MHD_djds_mat11
      private :: write_djds_matrices_MHD
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_write_djds_mat_MHD(FEM_prm, MHD_prop, solver_pack)
!
      use t_FEM_control_parameter
      use t_control_parameter
      use t_MHD_matrices_pack
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(MHD_matrices_pack), intent(in) :: solver_pack
!
!
      call write_djds_matrices_MHD(FEM_prm,                             &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    solver_pack%Vmatrix, solver_pack%Pmatrix,                     &
     &    solver_pack%Bmatrix, solver_pack%Fmatrix,                     &
     &    solver_pack%Tmatrix, solver_pack%Cmatrix)
!
      end subroutine s_write_djds_mat_MHD
!
! ----------------------------------------------------------------------
!
      subroutine write_djds_matrices_MHD                                &
     &         (FEM_prm, fl_prop, cd_prop, ht_prop, cp_prop,            &
     &          Vmatrix, Pmatrix, Bmatrix, Fmatrix, Tmatrix, Cmatrix)
!
      use t_FEM_control_parameter
      use t_physical_property
      use t_solver_djds_MHD
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(fluid_property), intent(in) :: fl_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(conductive_property), intent(in) :: cd_prop
      type(MHD_MG_matrix), intent(in) :: Vmatrix, Bmatrix
      type(MHD_MG_matrix), intent(in) :: Pmatrix, Fmatrix
      type(MHD_MG_matrix), intent(in) :: Tmatrix, Cmatrix
!
!
      if ( fl_prop%iflag_scheme .gt. id_no_evolution) then
        call write_MHD_djds_mat11                                       &
     &      (fhead_press_mat, FEM_PRM%CG11_param%METHOD,                &
     &       Pmatrix%nlevel_MG, Pmatrix%MG_comm_table,                  &
     &       Pmatrix%MG_DJDS_table, Pmatrix%mat_MG_DJDS)
      end if
!
      if ( fl_prop%iflag_scheme .ge. id_Crank_nicolson) then
        call write_MHD_djds_mat33(fhead_velo_mat, FEM_PRM%method_33,    &
     &       Vmatrix%nlevel_MG, Vmatrix%MG_comm_table,                  &
     &       Vmatrix%MG_DJDS_table, Vmatrix%mat_MG_DJDS)
      end if
!
      if ( ht_prop%iflag_scheme .ge. id_Crank_nicolson) then
        call write_MHD_djds_mat11                                       &
     &      (fhead_temp_mat, FEM_PRM%CG11_param%METHOD,                 &
     &       Tmatrix%nlevel_MG, Tmatrix%MG_comm_table,                  &
     &       Tmatrix%MG_DJDS_table, Tmatrix%mat_MG_DJDS)
      end if
!
      if ( cp_prop%iflag_scheme .ge. id_Crank_nicolson) then
        call write_MHD_djds_mat11                                       &
     &      (fhead_dscalar_mat, FEM_PRM%CG11_param%METHOD,              &
     &       Cmatrix%nlevel_MG, Cmatrix%MG_comm_table,                  &
     &       Cmatrix%MG_DJDS_table, Cmatrix%mat_MG_DJDS)
      end if
!
      if    (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution             &
     &  .or. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call write_MHD_djds_mat11                                       &
     &      (fhead_magp_mat, FEM_PRM%CG11_param%METHOD,                 &
     &       Fmatrix%nlevel_MG, Fmatrix%MG_comm_table,                  &
     &       Fmatrix%MG_DJDS_table, Fmatrix%mat_MG_DJDS)
      end if
!
      if    (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution             &
     &  .or. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call write_MHD_djds_mat33(fhead_magne_mat, FEM_PRM%method_33,   &
     &      Bmatrix%nlevel_MG, Bmatrix%MG_comm_table,                   &
     &      Bmatrix%MG_DJDS_table, Bmatrix%mat_MG_DJDS)
      end if
!
      end subroutine write_djds_matrices_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_MHD_djds_mat33(fhead_matrix, METHOD,             &
     &          num_MG_level, MG_comm, MG_djds_tbl, MG_mat33)
!
      use write_djds_matrix_struct
      use skip_comment_f
!
      character(len=kchara), intent(in) :: fhead_matrix, METHOD
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
          fname_tmp = add_int_suffix(i, fhead_matrix)
          fname =  add_process_id(my_rank, fname_tmp)
          open(id_mat_file, file=fname)
          call write_djds_mat33_comp_type(id_mat_file,  np_smp,         &
     &        MG_djds_tbl(i), MG_mat33(i) )
          call write_djds_mat_connect_type(id_mat_file,  np_smp,        &
     &        MG_comm(i), MG_djds_tbl(i), MG_mat33(i) )
          close(id_mat_file)
        end do
      else
        fname = add_process_id(my_rank, fhead_matrix)
        open(id_mat_file, file=fname)
        call write_djds_mat33_comp_type(id_mat_file, np_smp,            &
     &      MG_djds_tbl(0), MG_mat33(0))
        call write_djds_mat_connect_type(id_mat_file, np_smp,           &
     &      MG_comm(0), MG_djds_tbl(0), MG_mat33(0))
        close(id_mat_file)
      end if
!
      end subroutine write_MHD_djds_mat33
!
! ----------------------------------------------------------------------
!
      subroutine write_MHD_djds_mat11(fhead_matrix, METHOD,             &
     &          num_MG_level, MG_comm, MG_djds_tbl, MG_mat11)
!
      use write_djds_matrix_struct
      use skip_comment_f
!
      character(len=kchara), intent(in) :: fhead_matrix, METHOD
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
          fname_tmp = add_int_suffix(i, fhead_matrix)
          fname =  add_process_id(my_rank, fname_tmp)
          open(id_mat_file, file=fname)
          call write_djds_mat11_comp_type(id_mat_file,  np_smp,         &
     &        MG_djds_tbl(i), MG_mat11(i) )
          call write_djds_mat_connect_type(id_mat_file,  np_smp,        &
     &        MG_comm(i), MG_djds_tbl(i), MG_mat11(i) )
          close(id_mat_file)
        end do
      else
        fname = add_process_id(my_rank, fhead_matrix)
        open(id_mat_file, file=fname)
        call write_djds_mat11_comp_type(id_mat_file, np_smp,            &
     &      MG_djds_tbl(0), MG_mat11(0))
        call write_djds_mat_connect_type(id_mat_file, np_smp,           &
     &      MG_comm(0), MG_djds_tbl(0), MG_mat11(0))
        close(id_mat_file)
      end if
!
      end subroutine write_MHD_djds_mat11
!
! ----------------------------------------------------------------------
!
      end module write_djds_mat_MHD
