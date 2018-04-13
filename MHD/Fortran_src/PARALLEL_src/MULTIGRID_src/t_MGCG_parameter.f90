!t_MGCG_parameter.f90
!      module t_MGCG_parameter
!
!     Written by H. Matsui on Dec., 2008
!
!!      subroutine set_MGCG_parameter(MG_ctl, MG_param)
!!      subroutine set_MGCG_file_controls(n_level, MG_ctl, MG_file)
!!        type(MGCG_control), intent(inout) :: MG_ctl!
!!        type(MGCG_parameter), intent(inout) :: MG_param
!!        type(MGCG_file_list), intent(inout) :: MG_file
!
      module t_MGCG_parameter
!
      use m_precision
!
      implicit  none
!
!   parameteres for multigrid
!
      type MGCG_parameter
        character (len=kchara) :: METHOD_MG =  'CG'
        character (len=kchara) :: PRECOND_MG = 'DIAG'
        integer(kind=kint) ::     MID_ITR =   1
        integer(kind=kint) ::     MIN_ITR =  30
        real(kind=kreal) ::       EPS_MG =   1.0d-8
      end type MGCG_parameter
!
      type MGCG_file_list
        integer(kind = kint) :: nlevel_f
!
        character(len = kchara), allocatable :: MG_mesh_file_head(:)
!
        character(len = kchara), allocatable :: MG_f2c_tbl_head(:)
        character(len = kchara), allocatable :: MG_c2f_tbl_head(:)
        character(len = kchara), allocatable :: MG_f2c_eletbl_head(:)
!
        integer(kind = kint), allocatable :: ifmt_MG_mesh_file(:)
        integer(kind = kint), allocatable :: ifmt_MG_table_file(:)
      end type MGCG_file_list
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine alloc_MG_mesh_file_heads(n_level, MG_file)
!
      integer(kind = kint), intent(in) :: n_level
      type(MGCG_file_list), intent(inout) :: MG_file
!
!
      MG_file%nlevel_f = n_level
!
      allocate( MG_file%MG_mesh_file_head(MG_file%nlevel_f) )
!
      allocate( MG_file%MG_f2c_tbl_head(MG_file%nlevel_f) )
      allocate( MG_file%MG_c2f_tbl_head(MG_file%nlevel_f) )
      allocate( MG_file%MG_f2c_eletbl_head(MG_file%nlevel_f) )
!
      allocate( MG_file%ifmt_MG_mesh_file(MG_file%nlevel_f) )
      allocate( MG_file%ifmt_MG_table_file(MG_file%nlevel_f) )
!
      if(MG_file%nlevel_f .le. 0) return
      MG_file%ifmt_MG_mesh_file =  0
      MG_file%ifmt_MG_table_file = 0
!
      end subroutine alloc_MG_mesh_file_heads
!
!------------------------------------------------------------------
!
      subroutine dealloc_MG_mesh_file_heads(MG_file)
!
      type(MGCG_file_list), intent(inout) :: MG_file
!
!
      deallocate(MG_file%MG_mesh_file_head)
!
      deallocate(MG_file%MG_f2c_tbl_head, MG_file%MG_c2f_tbl_head)
      deallocate(MG_file%MG_f2c_eletbl_head)
      deallocate(MG_file%ifmt_MG_mesh_file, MG_file%ifmt_MG_table_file)
!
      MG_file%nlevel_f = 0
!
      end subroutine dealloc_MG_mesh_file_heads
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_MGCG_parameter(MG_ctl, MG_param)
!
      use m_machine_parameter
      use t_ctl_data_4_Multigrid
!
      type(MGCG_control), intent(in) :: MG_ctl
      type(MGCG_parameter), intent(inout) :: MG_param
!
!
      if (MG_ctl%MG_METHOD_ctl%iflag .gt. 0) then
        MG_param%METHOD_MG =     MG_ctl%MG_METHOD_ctl%charavalue
      end if
!
      if (MG_ctl%MG_PRECOND_ctl%iflag .gt. 0) then
        MG_param%PRECOND_MG =    MG_ctl%MG_PRECOND_ctl%charavalue
      end if
!
      if (MG_ctl%maxiter_mid_ctl%iflag .gt. 0) then
        MG_param%MID_ITR =    MG_ctl%maxiter_mid_ctl%intvalue
      end if
!
      if (MG_ctl%MG_residual_ctl%iflag .gt. 0) then
        MG_param%EPS_MG = MG_ctl%MG_residual_ctl%realvalue
      end if
!
      if (MG_ctl%maxiter_coarsest_ctl%iflag .gt. 0) then
        MG_param%MIN_ITR = MG_ctl%maxiter_coarsest_ctl%intvalue
      end if
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'METHOD_MG:  ', trim(MG_param%METHOD_MG)
        write(*,*) 'PRECOND_MG: ', trim(MG_param%PRECOND_MG)
        write(*,*) 'MID_ITR:    ', MG_param%MID_ITR
        write(*,*) 'MIN_ITR:    ', MG_param%MIN_ITR
        write(*,*) 'EPS_MG:     ', MG_param%EPS_MG
      end if
!
      end subroutine set_MGCG_parameter
!
!  ---------------------------------------------------------------------
!
      subroutine set_MGCG_file_controls(n_level, MG_ctl, MG_file)
!
      use calypso_mpi
      use m_machine_parameter
      use m_error_IDs
      use m_file_format_switch
      use t_ctl_data_4_Multigrid
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: n_level
      type(MGCG_control), intent(inout) :: MG_ctl
      type(MGCG_file_list), intent(inout) :: MG_file
!
      integer(kind = kint) :: i
!
!
      call alloc_MG_mesh_file_heads(n_level, MG_file)
!
      if (MG_file%nlevel_f .gt. 0) then
        if (MG_ctl%MG_mesh_prefix_ctl%num .eq. MG_file%nlevel_f) then
          MG_file%MG_mesh_file_head(1:MG_file%nlevel_f)                 &
     &          = MG_ctl%MG_mesh_prefix_ctl%c_tbl(1:MG_file%nlevel_f)
        else
          e_message = 'Set coarse mesh header'
          call calypso_MPI_abort(ierr_file, e_message)
        end if
!
        if(MG_ctl%MG_fine_2_coarse_tbl%icou .eq. MG_file%nlevel_f) then
          MG_file%MG_f2c_tbl_head(1:MG_file%nlevel_f)                   &
     &          = MG_ctl%MG_fine_2_coarse_tbl%c_tbl(1:MG_file%nlevel_f)
        else
          e_message = 'Set restriction table header'
          call calypso_MPI_abort(ierr_file, e_message)
        end if
!
        if(MG_ctl%MG_coarse_2_fine_tbl%icou .eq. MG_file%nlevel_f) then
          MG_file%MG_c2f_tbl_head(1:MG_file%nlevel_f)                   &
     &          = MG_ctl%MG_coarse_2_fine_tbl%c_tbl(1:MG_file%nlevel_f)
        else
          e_message = 'Set prolongation table header'
          call calypso_MPI_abort(ierr_file, e_message)
        end if
!
        if (MG_ctl%MG_f2c_ele_tbl_ctl%icou .eq. MG_file%nlevel_f) then
          MG_file%MG_f2c_eletbl_head(1:MG_file%nlevel_f)                &
     &          = MG_ctl%MG_f2c_ele_tbl_ctl%c_tbl(1:MG_file%nlevel_f)
        end if
!
        if (MG_ctl%MG_mesh_fmt_ctl%icou .eq. MG_file%nlevel_f) then
          call choose_file_format_array(MG_file%nlevel_f,               &
     &        MG_ctl%MG_mesh_fmt_ctl, MG_file%ifmt_MG_mesh_file)
        else
          e_message = 'Set mesh file formats for MG'
          call calypso_MPI_abort(ierr_file, e_message)
        end if
!
        if(MG_ctl%MG_table_fmt_ctl%icou .eq. MG_file%nlevel_f) then
          call choose_file_format_array(MG_file%nlevel_f,               &
     &        MG_ctl%MG_table_fmt_ctl, MG_file%ifmt_MG_table_file)
        else
          e_message = 'Set interpolation table file formats for MG'
          call calypso_MPI_abort(ierr_file, e_message)
        end if
      end if
!
      call dealloc_control_array_chara(MG_ctl%MG_mesh_prefix_ctl)
      call dealloc_control_array_chara(MG_ctl%MG_fine_2_coarse_tbl)
      call dealloc_control_array_chara(MG_ctl%MG_coarse_2_fine_tbl)
      call dealloc_control_array_chara(MG_ctl%MG_f2c_ele_tbl_ctl)
      call dealloc_control_array_chara(MG_ctl%MG_mesh_fmt_ctl)
      call dealloc_control_array_chara(MG_ctl%MG_table_fmt_ctl)
!
      if (iflag_debug .gt. 0) then
        do i = 1, MG_file%nlevel_f
          write(*,*) 'MG_mesh_file_head',                               &
     &                trim(MG_file%MG_mesh_file_head(i))
!
          write(*,*) 'MG_f2c_tbl_head: ',                               &
     &          trim(MG_file%MG_f2c_tbl_head(i))
          write(*,*) 'MG_c2f_tbl_head: ',                               &
     &          trim(MG_file%MG_c2f_tbl_head(i))
!
          write(*,*) 'MG_f2c_eletbl_head: ',                            &
     &          trim(MG_file%MG_f2c_eletbl_head(i))
        end do
      end if
!
      end subroutine set_MGCG_file_controls
!
!  ---------------------------------------------------------------------
!
      end module t_MGCG_parameter
