!m_ctl_parameter_Multigrid.f90
!      module m_ctl_parameter_Multigrid
!
!     Written by H. Matsui on Dec., 2008
!
!      subroutine set_ctl_data_4_Multigrid
!
      module m_ctl_parameter_Multigrid
!
      use m_precision
      use m_error_IDs
!
      implicit  none
!
!   parameteres for multigrid
!
      character (len=kchara) :: METHOD_MG =  'CG'
      character (len=kchara) :: PRECOND_MG = 'DIAG'
      integer(kind=kint) ::     itr_MG_mid =     2
      integer(kind=kint) ::     itr_MG_lowest =  30
      real(kind=kreal) ::       EPS_MG =         1.0d-8
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine set_ctl_data_4_Multigrid
!
      use calypso_mpi
      use m_machine_parameter
      use m_file_format_switch
      use m_type_AMG_data
      use m_type_AMG_mesh
      use m_ctl_data_4_Multigrid
      use set_parallel_file_name
!
      integer(kind = kint) :: i
!
!
      if (i_num_MG_level .gt. 0) then
        num_MG_level = num_multigrid_level_ctl
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
        if(num_MG_subdomain_ctl%num .ne. num_MG_level) then
          write(e_message,'(a)')                                        &
     &            'set correct level for MG subdomains'
          call calypso_MPI_abort(ierr_CG, e_message)
        end if
!
        MG_mpi(1:num_MG_level)%nprocs                                   &
     &         = num_MG_subdomain_ctl%ivec(1:num_MG_level)
        call dealloc_control_array_int(num_MG_subdomain_ctl)
!
        if (MG_mesh_prefix_ctl%num .eq. num_MG_level) then
          MG_mesh_file_head(1:num_MG_level)                             &
     &              = MG_mesh_prefix_ctl%c_tbl(1:num_MG_level)
          call dealloc_control_array_chara(MG_mesh_prefix_ctl)
        else
          e_message = 'Set coarse mesh header'
          call calypso_MPI_abort(ierr_file, e_message)
        end if
!
        if (MG_fine_2_coarse_tbl_ctl%icou .eq. num_MG_level) then
          MG_f2c_tbl_head(1:num_MG_level)                               &
     &              = MG_fine_2_coarse_tbl_ctl%c_tbl(1:num_MG_level)
          call dealloc_control_array_chara(MG_fine_2_coarse_tbl_ctl)
        else
          e_message = 'Set restriction table header'
          call calypso_MPI_abort(ierr_file, e_message)
        end if
!
        if (MG_coarse_2_fine_tbl_ctl%icou .eq. num_MG_level) then
          MG_c2f_tbl_head(1:num_MG_level)                               &
     &              = MG_coarse_2_fine_tbl_ctl%c_tbl(1:num_MG_level)
          call dealloc_control_array_chara(MG_coarse_2_fine_tbl_ctl)
        else
          e_message = 'Set prolongation table header'
          call calypso_MPI_abort(ierr_file, e_message)
        end if
!
        if (MG_f2c_ele_tbl_ctl%icou .eq. num_MG_level) then
          MG_f2c_eletbl_head(1:num_MG_level)                            &
     &              = MG_f2c_ele_tbl_ctl%c_tbl(1:num_MG_level)
          call dealloc_control_array_chara(MG_f2c_ele_tbl_ctl)
          iflag_MG_commute_by_ele = 1
        end if
!
!
        if (MG_elem_prefix_ctl%icou .eq. num_MG_level) then
          iflag_MG_elem_file(1:num_MG_level) = 1
          MG_elem_file_head(1:num_MG_level)                             &
     &              = MG_elem_prefix_ctl%c_tbl(1:num_MG_level)
          call dealloc_control_array_chara(MG_elem_prefix_ctl)
        else
          iflag_MG_elem_file(1:num_MG_level) = 0
        end if
!
        if (MG_surf_prefix_ctl%icou .eq. num_MG_level) then
          iflag_MG_surf_file(1:num_MG_level) = 1
          MG_surf_file_head(1:num_MG_level)                             &
     &              = MG_surf_prefix_ctl%c_tbl(1:num_MG_level)
          call dealloc_control_array_chara(MG_surf_prefix_ctl)
        else
          iflag_MG_surf_file(1:num_MG_level) = 0
        end if
!
        if (MG_edge_prefix_ctl%icou .eq. num_MG_level) then
          iflag_MG_edge_file(1:num_MG_level) = 1
          MG_edge_file_head(1:num_MG_level)                             &
     &              = MG_edge_prefix_ctl%c_tbl(1:num_MG_level)
          call dealloc_control_array_chara(MG_edge_prefix_ctl)
        else
          iflag_MG_edge_file(1:num_MG_level) = 0
        end if
!
        if (MG_mesh_fmt_ctl%icou .eq. num_MG_level) then
          do i = 1, num_MG_level
            call choose_file_format(MG_mesh_fmt_ctl%c_tbl(i),           &
     &          MG_mesh_fmt_ctl%icou, ifmt_MG_mesh_file(i))
          end do
          call dealloc_control_array_chara(MG_mesh_fmt_ctl)
        else
          e_message = 'Set mesh file formats for MG'
          call calypso_MPI_abort(ierr_file, e_message)
        end if
!
        if(MG_table_fmt_ctl%icou .eq. num_MG_level) then
          do i = 1, num_MG_level
            call choose_file_format(MG_table_fmt_ctl%c_tbl(i),          &
     &        MG_table_fmt_ctl%icou, ifmt_MG_table_file(i))
          end do
          call dealloc_control_array_chara(MG_table_fmt_ctl)
        else
          e_message = 'Set interpolation table file formats for MG'
          call calypso_MPI_abort(ierr_file, e_message)
        end if
      end if
!
!
      if (i_MG_METHOD .gt. 0)   METHOD_MG =      MG_METHOD_ctl
      if (i_MG_PRECOND .gt. 0)  PRECOND_MG =    MG_PRECOND_ctl
      if (i_maxiter_mid .gt. 0) itr_MG_mid =    maxiter_mid_ctl
      if (i_MG_residual .gt. 0) EPS_MG =        MG_residual_ctl
!
      if (i_maxiter_coarsest .gt. 0) then
        itr_MG_lowest = maxiter_coarsest_ctl
      end if
!
!
!
        if (iflag_debug .gt. 0) then
          do i = 1, num_MG_level
            write(*,*) 'MG_mesh_file_head', MG_mpi(i)%nprocs,           &
     &                trim(MG_mesh_file_head(i))
!
            write(*,*) 'MG_f2c_tbl_head: ', trim(MG_f2c_tbl_head(i))
            write(*,*) 'MG_c2f_tbl_head: ', trim(MG_c2f_tbl_head(i))
!
            write(*,*) 'MG_f2c_eletbl_head: ',                          &
     &                                   trim(MG_f2c_eletbl_head(i))
!
            if (iflag_MG_elem_file(i) .gt. 0) write(*,*)                &
     &               'MG_elem_file_head', trim(MG_elem_file_head(i))
            if (iflag_MG_surf_file(i) .gt. 0) write(*,*)                &
     &               'MG_surf_file_head', trim(MG_surf_file_head(i))
            if (iflag_MG_edge_file(i) .gt. 0) write(*,*)                &
     &               'MG_edge_file_head', trim(MG_edge_file_head(i))
          end do
!
          write(*,*) 'METHOD_MG:     ', trim(METHOD_MG)
          write(*,*) 'PRECOND_MG:    ', trim(PRECOND_MG)
          write(*,*) 'itr_MG_mid:    ', itr_MG_mid
          write(*,*) 'itr_MG_lowest: ', itr_MG_lowest
          write(*,*) 'EPS_MG:        ', EPS_MG
!
        end if
!
!
      end subroutine set_ctl_data_4_Multigrid
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_parameter_Multigrid
