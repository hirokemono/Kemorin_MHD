!
!      module input_MG_data
!
!        programmed by H. Matsui on Dec., 2008
!
!!      subroutine input_MG_mesh(MG_file, MGCG_WK, MGCG_FEM, mesh_file)
!!        type(MGCG_file_list), intent(in) :: MG_file
!!        type(MGCG_data), intent(in) :: MGCG_WK
!!        type(mesh_4_MGCG), intent(inout) :: MGCG_FEM
!!        type(field_IO_params), intent(inout) ::  mesh_file
!!      subroutine input_MG_itp_tables                                  &
!!     &          (MG_file, MGCG_WK, MGCG_FEM, MG_itp)
!!        type(MGCG_file_list), intent(in) :: MG_file
!!        type(MGCG_data), intent(in) :: MGCG_WK
!!        type(MG_itp_table), intent(inout) :: MG_itp(num_MG_level)
!
      module input_MG_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use t_MGCG_parameter
      use t_MGCG_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine input_MG_mesh(MG_file, MGCG_WK, MGCG_FEM, mesh_file)
!
      use t_file_IO_parameter
      use mpi_load_mesh_data
      use load_mesh_data
      use element_file_IO
!
      type(MGCG_file_list), intent(in) :: MG_file
      type(MGCG_data), intent(in) :: MGCG_WK
      type(mesh_4_MGCG), intent(inout) :: MGCG_FEM
      type(field_IO_params), intent(inout) ::  mesh_file
!
      integer(kind = kint) :: i_level
!
!
      do i_level = 1, MG_file%nlevel_f
        mesh_file%iflag_format = MG_file%ifmt_MG_mesh_file(i_level)
        mesh_file%file_prefix = MG_file%MG_mesh_file_head(i_level)
        call mpi_input_mesh(mesh_file, MGCG_WK%MG_mpi(i_level)%nprocs,  &
     &      MGCG_FEM%MG_mesh(i_level))
      end do
!
      end subroutine input_MG_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine input_MG_itp_tables                                    &
     &          (MG_file, MGCG_WK, MGCG_FEM, MG_itp)
!
      use m_interpolate_table_IO
      use itp_table_IO_select_4_zlib
!
      type(MGCG_file_list), intent(in) :: MG_file
      type(MGCG_data), intent(in) :: MGCG_WK
      type(mesh_4_MGCG), intent(inout) :: MGCG_FEM
      type(MG_itp_table), intent(inout)                                 &
     &     :: MG_itp(MGCG_WK%num_MG_level)
!
      integer(kind = kint) :: i_level
!
!
      do i_level = 1, MG_file%nlevel_f
        if(my_rank.lt.MGCG_WK%MG_mpi(i_level-1)%nprocs                  &
     &      .or. i_level .eq. 1) then
          write(*,*) 'MG_f2c_tbl_head format', ifmt_itp_table_file
          table_file_header = MG_file%MG_f2c_tbl_head(i_level)
          ifmt_itp_table_file = MG_file%ifmt_MG_table_file(i_level)
          call load_interpolate_table(my_rank, MG_itp(i_level)%f2c)
        else
          call load_zero_interpolate_table(MG_itp(i_level)%f2c)
        end if
!
      end do
!
!
!
      do i_level = 1, MG_file%nlevel_f
        if(my_rank .lt. MGCG_WK%MG_mpi(i_level-1)%nprocs                &
     &      .or. i_level .eq. 1) then
          write(*,*) 'MG_c2f_tbl_head format', ifmt_itp_table_file
          table_file_header = MG_file%MG_c2f_tbl_head(i_level)
          ifmt_itp_table_file = MG_file%ifmt_MG_table_file(i_level)
          call load_interpolate_table(my_rank, MG_itp(i_level)%c2f)
        else
          call load_zero_interpolate_table(MG_itp(i_level)%c2f)
        end if
!
      end do
!
!
!
      if(MGCG_FEM%iflag_MG_commute_by_ele .gt. 0) then
        do i_level = 1, MG_file%nlevel_f
          if(my_rank.lt.MGCG_WK%MG_mpi(i_level-1)%nprocs                &
     &      .or. i_level .eq. 1) then
            table_file_header = MG_file%MG_f2c_eletbl_head(i_level)
            call load_interpolate_table                                 &
     &         (my_rank, MGCG_FEM%MG_c2f_ele_tbl(i_level) )
          else
            call load_zero_interpolate_table                            &
     &         (MGCG_FEM%MG_c2f_ele_tbl(i_level))
          end if
!
        end do
      end if
!
      end subroutine input_MG_itp_tables
!
!  ---------------------------------------------------------------------
!
      end module input_MG_data
