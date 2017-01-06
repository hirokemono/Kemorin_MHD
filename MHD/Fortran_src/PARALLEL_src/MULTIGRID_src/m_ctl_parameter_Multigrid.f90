!m_ctl_parameter_Multigrid.f90
!      module m_ctl_parameter_Multigrid
!
!     Written by H. Matsui on Dec., 2008
!
!!      subroutine set_ctl_data_4_Multigrid(MG_ctl)
!!        type(MGCG_control), intent(inout) :: MG_ctl!
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
      integer(kind=kint) ::     itr_MG_mid =     1
      integer(kind=kint) ::     itr_MG_lowest =  30
      real(kind=kreal) ::       EPS_MG =         1.0d-8
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine set_ctl_data_4_Multigrid(MG_ctl)
!
      use calypso_mpi
      use m_machine_parameter
      use m_file_format_switch
      use m_type_AMG_data
      use m_type_AMG_mesh
      use t_ctl_data_4_Multigrid
      use set_parallel_file_name
!
      type(MGCG_control), intent(inout) :: MG_ctl
!
      integer(kind = kint) :: i
!
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
      if (MG_ctl%MG_METHOD_ctl%iflag .gt. 0) then
        METHOD_MG =     MG_ctl%MG_METHOD_ctl%charavalue
      end if
!
      if (MG_ctl%MG_PRECOND_ctl%iflag .gt. 0) then
        PRECOND_MG =    MG_ctl%MG_PRECOND_ctl%charavalue
      end if
!
      if (MG_ctl%maxiter_mid_ctl%iflag .gt. 0) then
        itr_MG_mid =    MG_ctl%maxiter_mid_ctl%intvalue
      end if
!
      if (MG_ctl%MG_residual_ctl%iflag .gt. 0) then
        EPS_MG = MG_ctl%MG_residual_ctl%realvalue
      end if
!
      if (MG_ctl%maxiter_coarsest_ctl%iflag .gt. 0) then
        itr_MG_lowest = MG_ctl%maxiter_coarsest_ctl%intvalue
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
