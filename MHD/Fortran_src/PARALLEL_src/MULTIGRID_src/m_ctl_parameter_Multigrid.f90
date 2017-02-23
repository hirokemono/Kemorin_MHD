!m_ctl_parameter_Multigrid.f90
!      module m_ctl_parameter_Multigrid
!
!     Written by H. Matsui on Dec., 2008
!
!!      subroutine set_ctl_data_4_Multigrid(MG_ctl, MG_param)
!!        type(MGCG_control), intent(inout) :: MG_ctl!
!!        type(FEM_MHD_paremeters), intent(inout) :: MG_param
!
      module m_ctl_parameter_Multigrid
!
      use m_precision
      use m_error_IDs
!
      implicit  none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine set_ctl_data_4_Multigrid(MG_ctl, MG_param)
!
      use calypso_mpi
      use m_machine_parameter
      use m_file_format_switch
      use m_type_AMG_data
      use m_type_AMG_mesh
      use t_MGCG_parameter
      use t_ctl_data_4_Multigrid
      use set_parallel_file_name
!
      type(MGCG_control), intent(inout) :: MG_ctl
      type(MGCG_parameter), intent(inout) :: MG_param
!
      integer(kind = kint) :: i
!
!
      call set_MGCG_parameter(MG_ctl, MG_param)
!
      if (MG_ctl%num_multigrid_level_ctl%iflag .gt. 0) then
        num_MG_level = MG_ctl%num_multigrid_level_ctl%intvalue
      else
        num_MG_level = 0
      end if
!
      call allocate_MG_mesh_file_heads
!
      if (num_MG_level .gt. max_MG_level) then
          write(e_message,*)                                            &
     &           'Resize maximum MG level to ', num_MG_level
          call calypso_MPI_abort(ierr_CG, e_message)
      end if
!
      if (num_MG_level .gt. 0) then
        if(MG_ctl%num_MG_subdomain_ctl%num .ne. num_MG_level) then
          write(e_message,'(a)')                                        &
     &            'set correct level for MG subdomains'
          call calypso_MPI_abort(ierr_CG, e_message)
        end if
!
        MG_mpi(1:num_MG_level)%nprocs                                   &
     &         = MG_ctl%num_MG_subdomain_ctl%ivec(1:num_MG_level)
        call dealloc_control_array_int(MG_ctl%num_MG_subdomain_ctl)
!
        if (MG_ctl%MG_mesh_prefix_ctl%num .eq. num_MG_level) then
          MG_mesh_file_head(1:num_MG_level)                             &
     &              = MG_ctl%MG_mesh_prefix_ctl%c_tbl(1:num_MG_level)
          call dealloc_control_array_chara(MG_ctl%MG_mesh_prefix_ctl)
        else
          e_message = 'Set coarse mesh header'
          call calypso_MPI_abort(ierr_file, e_message)
        end if
!
        if (MG_ctl%MG_fine_2_coarse_tbl%icou .eq. num_MG_level) then
          MG_f2c_tbl_head(1:num_MG_level)                               &
     &              = MG_ctl%MG_fine_2_coarse_tbl%c_tbl(1:num_MG_level)
          call dealloc_control_array_chara(MG_ctl%MG_fine_2_coarse_tbl)
        else
          e_message = 'Set restriction table header'
          call calypso_MPI_abort(ierr_file, e_message)
        end if
!
        if (MG_ctl%MG_coarse_2_fine_tbl%icou .eq. num_MG_level) then
          MG_c2f_tbl_head(1:num_MG_level)                               &
     &              = MG_ctl%MG_coarse_2_fine_tbl%c_tbl(1:num_MG_level)
          call dealloc_control_array_chara(MG_ctl%MG_coarse_2_fine_tbl)
        else
          e_message = 'Set prolongation table header'
          call calypso_MPI_abort(ierr_file, e_message)
        end if
!
        if (MG_ctl%MG_f2c_ele_tbl_ctl%icou .eq. num_MG_level) then
          MG_f2c_eletbl_head(1:num_MG_level)                            &
     &              = MG_ctl%MG_f2c_ele_tbl_ctl%c_tbl(1:num_MG_level)
          call dealloc_control_array_chara(MG_ctl%MG_f2c_ele_tbl_ctl)
          iflag_MG_commute_by_ele = 1
        end if
!
        if (MG_ctl%MG_mesh_fmt_ctl%icou .eq. num_MG_level) then
          call choose_file_format_array(num_MG_level,                   &
     &        MG_ctl%MG_mesh_fmt_ctl, ifmt_MG_mesh_file)
          call dealloc_control_array_chara(MG_ctl%MG_mesh_fmt_ctl)
        else
          e_message = 'Set mesh file formats for MG'
          call calypso_MPI_abort(ierr_file, e_message)
        end if
!
        if(MG_ctl%MG_table_fmt_ctl%icou .eq. num_MG_level) then
          call choose_file_format_array(num_MG_level,                   &
     &        MG_ctl%MG_table_fmt_ctl, ifmt_MG_table_file)
          call dealloc_control_array_chara(MG_ctl%MG_table_fmt_ctl)
        else
          e_message = 'Set interpolation table file formats for MG'
          call calypso_MPI_abort(ierr_file, e_message)
        end if
      end if
!
!
      if (iflag_debug .gt. 0) then
        do i = 1, num_MG_level
          write(*,*) 'MG_mesh_file_head', MG_mpi(i)%nprocs,             &
     &                trim(MG_mesh_file_head(i))
!
          write(*,*) 'MG_f2c_tbl_head: ', trim(MG_f2c_tbl_head(i))
          write(*,*) 'MG_c2f_tbl_head: ', trim(MG_c2f_tbl_head(i))
!
          write(*,*) 'MG_f2c_eletbl_head: ',                            &
     &          trim(MG_f2c_eletbl_head(i))
        end do
      end if
!
      end subroutine set_ctl_data_4_Multigrid
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_parameter_Multigrid
