!
!      module m_type_AMG_mesh
!
!     Written by H. Matsui on Dec., 2008
!
!      subroutine allocate_MG_mesh_file_heads
!      subroutine deallocate_MG_mesh_file_heads
!
      module m_type_AMG_mesh
!
      use m_precision
!
      use m_type_AMG_data
      use t_mesh_data
      use t_interpolate_table
      use t_jacobians
      use t_table_FEM_const
      use t_work_FEM_integration
!
      implicit  none
!
!
      type(mesh_data), target, save :: MG_mesh(max_MG_level)
      type(element_geometry), save ::  MG_ele_mesh(max_MG_level)
!   mesh data structure
!
      integer(kind = kint), save :: iflag_MG_commute_by_ele = 0
      type(interpolate_table), save :: MG_c2f_ele_tbl(max_MG_level)
!   interpolation table structure for elemnent averaging
!
      type(jacobians_type), save :: MG_jacobians(max_MG_level)
!   jacobians for mesh
!
      type(tables_4_FEM_assembles), save ::    MG_FEM_tbl(max_MG_level)
      type(arrays_finite_element_mat), save :: MG_FEM_mat(max_MG_level)
!   table for FEM assemble
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine set_ctl_data_4_Multigrid(MG_ctl, MG_param, MG_file)
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
      use m_file_format_switch
      use m_type_AMG_data
      use t_MGCG_parameter
      use t_ctl_data_4_Multigrid
      use set_parallel_file_name
!
      type(MGCG_control), intent(inout) :: MG_ctl
      type(MGCG_parameter), intent(inout) :: MG_param
      type(MGCG_file_list), intent(inout) :: MG_file
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
        if (MG_ctl%MG_f2c_ele_tbl_ctl%icou .eq. MG_file%nlevel_f) then
          iflag_MG_commute_by_ele = 1
        end if
      end if
!
      if (iflag_debug .gt. 0) then
        do i = 1, MG_file%nlevel_f
          write(*,*) '# of domains for level ', i, ':  ',              &
     &               MG_mpi(i)%nprocs
        end do
      end if
!
      call set_MGCG_file_controls(num_MG_level, MG_ctl, MG_file)
!
      end subroutine set_ctl_data_4_Multigrid
!
!  ---------------------------------------------------------------------
!
      end module m_type_AMG_mesh
