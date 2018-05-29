!set_fline_control.f90
!      module set_fline_control
!
!     Written by H. Matsui on Aug., 2011
!
!!      subroutine s_set_fline_control(mesh, group, nod_fld,            &
!!     &          num_fline, fline_ctls, fline_prm, fline_src)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_controls), intent(inout) :: fline_ctls
!!        type(fieldline_paramters), intent(inout) :: fline_prm
!!        type(fieldline_source), intent(inout) :: fline_src
!
      module set_fline_control
!
      use m_precision
!
      use m_machine_parameter
!
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_phys_data
!
      implicit none
!
      private :: read_control_4_fline
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_fline_control(mesh, group, nod_fld,              &
     &          num_fline, fline_ctls, fline_prm, fline_src)
!
      use t_control_data_flines
      use t_control_params_4_fline
      use t_source_of_filed_line
      use set_control_each_fline
!
      integer(kind = kint), intent(in) :: num_fline
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
!
      type(fieldline_controls), intent(inout) :: fline_ctls
      type(fieldline_paramters), intent(inout) :: fline_prm
      type(fieldline_source), intent(inout) :: fline_src
!
      integer(kind = kint) :: i
!
!
      ctl_file_code = fline_ctl_file_code
!
      call alloc_control_params_fline(num_fline, fline_prm)
      call alloc_local_start_grp_num(num_fline, fline_src)
!
      do i = 1, num_fline
        call read_control_4_fline                                       &
     &     (hd_fline_ctl, fline_ctls%fname_fline_ctl(i),                &
     &      fline_ctls%fline_ctl_struct(i))
      end do
!
      do i = 1, num_fline
        call count_control_4_fline(i, fline_ctls%fline_ctl_struct(i),   &
     &      mesh%ele, group%ele_grp, group%surf_grp,                    &
     &      fline_prm, fline_src)
      end do
!
      call alloc_iflag_fline_used_ele(num_fline, mesh%ele, fline_prm)
      call alloc_fline_starts_ctl(num_fline, fline_prm)
      call alloc_local_start_grp_item(num_fline, fline_src)
!
      do i = 1, num_fline
        call set_control_4_fline(i, fline_ctls%fline_ctl_struct(i),     &
     &      mesh%ele, group%ele_grp, group%surf_grp, nod_fld,           &
     &      fline_prm, fline_src)
        call set_iflag_fline_used_ele                                   &
     &     (i, mesh%ele, group%ele_grp, fline_prm)
        call deallocate_cont_dat_fline(fline_ctls%fline_ctl_struct(i))
!
        if(iflag_debug .gt. 0) then
          call check_control_params_fline(i, fline_prm)
        end if
      end do
!
      call dealloc_fline_fhead_ctl(fline_ctls)
!
      end subroutine s_set_fline_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_4_fline                                   &
     &         (hd_fline_ctl, fname_fline_ctl, fline_ctl_struct)
!
      use calypso_mpi
      use t_control_data_4_fline
!
      character(len = kchara), intent(in) :: hd_fline_ctl
      character(len = kchara), intent(in) :: fname_fline_ctl
      type(fline_ctl), intent(inout)  :: fline_ctl_struct
!
      if(fname_fline_ctl .eq. 'NO_FILE') return
      call reset_fline_control_flags(fline_ctl_struct)
!
      if(my_rank .eq. 0) then
        open(fline_ctl_file_code, file=fname_fline_ctl,                 &
     &         status='old')
        call load_ctl_label_and_line
        call read_field_line_ctl                                        &
     &     (hd_fline_ctl, fline_ctl_struct)
        close(fline_ctl_file_code)
      end if
!
      call bcast_field_line_ctl(fline_ctl_struct)
!
      end subroutine read_control_4_fline
!
!  ---------------------------------------------------------------------
!
      end module set_fline_control
