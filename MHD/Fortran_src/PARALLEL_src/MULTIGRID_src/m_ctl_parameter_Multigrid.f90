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
      if ( num_MG_level .gt. max_MG_level) then
          write(e_message,*)                                            &
     &           'Resize maximum MG level to ', num_MG_level
          call calypso_MPI_abort(999, e_message)
      end if
!
      if (num_MG_level .gt. 0) then
        MG_vector(1:num_MG_level)%nprocs                                &
    &         = num_MG_subdomain_ctl(1:num_MG_level)
!
        if (i_MG_mesh_header .eq. num_MG_level) then
          MG_mesh_file_head(1:num_MG_level)                             &
     &              = MG_mesh_file_head_ctl(1:num_MG_level)
        else
          e_message = 'Set coarse mesh header'
          call calypso_MPI_abort(1000, e_message)
        end if
!
        if (i_MG_fine_2_coarse_tbl .eq. num_MG_level) then
          MG_f2c_tbl_head(1:num_MG_level)                               &
     &              = MG_fine_2_coarse_tbl_ctl(1:num_MG_level)
        else
          e_message = 'Set restriction table header'
          call calypso_MPI_abort(1001, e_message)
        end if
!
        if (i_MG_coarse_2_fine_tbl .eq. num_MG_level) then
          MG_c2f_tbl_head(1:num_MG_level)                               &
     &              = MG_coarse_2_fine_tbl_ctl(1:num_MG_level)
        else
          e_message = 'Set prolongation table header'
          call calypso_MPI_abort(1002, e_message)
        end if
!
        if (i_MG_f2c_ele_tbl .eq. num_MG_level) then
          MG_f2c_eletbl_head(1:num_MG_level)                            &
     &              = MG_f2c_ele_tbl_ctl(1:num_MG_level)
          iflag_MG_commute_by_ele = 1
        end if
!
!
        if (i_MG_elem_header .eq. num_MG_level) then
          iflag_MG_elem_file(1:num_MG_level) = 1
          MG_elem_file_head(1:num_MG_level)                             &
     &              = MG_elem_file_head_ctl(1:num_MG_level)
        else
          iflag_MG_elem_file(1:num_MG_level) = 0
        end if
!
        if (i_MG_surf_header .eq. num_MG_level) then
          iflag_MG_surf_file(1:num_MG_level) = 1
          MG_surf_file_head(1:num_MG_level)                             &
     &              = MG_surf_file_head_ctl(1:num_MG_level)
        else
          iflag_MG_surf_file(1:num_MG_level) = 0
        end if
!
        if (i_MG_edge_header .eq. num_MG_level) then
          iflag_MG_edge_file(1:num_MG_level) = 1
          MG_edge_file_head(1:num_MG_level)                             &
     &              = MG_edge_file_head_ctl(1:num_MG_level)
        else
          iflag_MG_edge_file(1:num_MG_level) = 0
        end if
!
        do i = 1, num_MG_level
          call choose_file_format(MG_mesh_file_fmt_ctl(i),              &
     &        i_MG_mesh_file_fmt, ifmt_MG_mesh_file(i))
          call choose_file_format(MG_table_file_fmt_ctl(i),             &
     &        i_MG_tbl_file_fmt, ifmt_MG_table_file(i))
         end do
!
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
            write(*,*) 'MG_mesh_file_head', MG_vector(i)%nprocs,        &
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
