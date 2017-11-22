!set_fline_control.f90
!      module set_fline_control
!
!     Written by H. Matsui on Aug., 2011
!
!!      subroutine s_set_fline_control                                  &
!!     &         (ele, ele_grp, sf_grp, nod_fld, fline_ctls, fline_src)
!!        type(element_data), intent(in) :: ele
!!        type(group_data), intent(in) :: ele_grp
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_controls), intent(inout) :: fline_ctls
!!        type(fieldline_source), intent(inout) :: fline_src
!
      module set_fline_control
!
      use m_precision
!
      use m_machine_parameter
      use m_control_params_4_fline
!
      use t_geometry_data
      use t_group_data
      use t_phys_data
!
      implicit none
!
      character(len=kchara) :: hd_fline_ctl = 'field_line'
      private :: hd_fline_ctl
!
      private :: read_control_4_fline
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_fline_control                                    &
     &         (ele, ele_grp, sf_grp, nod_fld, fline_ctls, fline_src)
!
      use t_control_data_flines
      use t_source_of_filed_line
      use set_control_each_fline
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data), intent(in) :: nod_fld
!
      type(fieldline_controls), intent(inout) :: fline_ctls
      type(fieldline_source), intent(inout) :: fline_src
!
      integer(kind = kint) :: i
!
!
      ctl_file_code = fline_ctl_file_code
!
      call allocate_control_params_fline
      call alloc_local_start_grp_num(fline_src)
!
      do i = 1, num_fline
        call read_control_4_fline(fline_ctls%fname_fline_ctl(i),        &
     &      fline_ctls%fline_ctl_struct(i))
      end do
!
      do i = 1, num_fline
        call count_control_4_fline(i, fline_ctls%fline_ctl_struct(i),   &
     &      ele, ele_grp, sf_grp, fline_src)
      end do
!
      call allocate_iflag_fline_used_ele(ele%numele)
      call allocate_fline_starts_ctl
      call alloc_local_start_grp_item(fline_src)
!
      do i = 1, num_fline
        call set_control_4_fline(i, fline_ctls%fline_ctl_struct(i),     &
     &      ele, ele_grp, sf_grp, nod_fld, fline_src)
        call set_iflag_fline_used_ele(i, ele, ele_grp)
        call deallocate_cont_dat_fline(fline_ctls%fline_ctl_struct(i))
!
        if(iflag_debug .gt. 0) call check_control_params_fline(i)
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
     &         (fname_fline_ctl, fline_ctl_struct)
!
      use calypso_mpi
      use t_control_data_4_fline
!
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
