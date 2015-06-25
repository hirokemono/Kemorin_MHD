!
!      module set_pvr_control
!
!     Written by H. Matsui on May., 2006
!
!!      subroutine s_set_pvr_control                                    &
!!     &       (num_pvr, num_mat, mat_name, num_nod_phys, phys_nod_name,&
!!     &        file_params, fld_params, view_params, color_params,     &
!!     &        cbar_params)
!
      module set_pvr_control
!
      use m_precision
!
      use m_control_data_pvrs
      use m_control_data_4_pvr
!
      implicit none
!
      integer(kind = kint), parameter :: pvr_ctl_file_code = 11
!
      private :: pvr_ctl_file_code
      private :: read_control_pvr
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_pvr_control                                      &
     &       (num_pvr, num_mat, mat_name, num_nod_phys, phys_nod_name,  &
     &        file_params, fld_params, view_params, color_params,       &
     &        cbar_params)
!
      use t_control_params_4_pvr
      use set_control_each_pvr
      use set_field_comp_for_viz
!
      integer(kind = kint), intent(in) :: num_mat
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      integer(kind = kint), intent(in) :: num_pvr
      type(pvr_output_parameter), intent(inout) :: file_params(num_pvr)
      type(pvr_field_parameter), intent(inout) :: fld_params(num_pvr)
      type(pvr_view_parameter), intent(inout) :: view_params(num_pvr)
      type(pvr_colormap_parameter), intent(inout)                       &
     &                  :: color_params(num_pvr)
      type(pvr_colorbar_parameter), intent(inout)                       &
     &                  :: cbar_params(num_pvr)
!
      integer(kind = kint) :: i_pvr
!
!
      ctl_file_code = pvr_ctl_file_code
!
      call allocate_pvr_ctl_struct
!
      do i_pvr = 1, num_pvr
        call read_control_pvr(i_pvr)
      end do
!
      do i_pvr = 1, num_pvr
        if(iflag_debug .gt. 0) write(*,*) 'PVR parameters for', i_pvr
        call set_pvr_file_control(pvr_ctl_struct(i_pvr),                &
     &      num_nod_phys, phys_nod_name, file_params(i_pvr))
!
        if(iflag_debug .gt. 0) write(*,*) 'set_control_pvr', i_pvr
        call set_control_pvr(pvr_ctl_struct(i_pvr), num_mat,            &
     &      mat_name, num_nod_phys, phys_nod_name,                      &
     &      fld_params(i_pvr), view_params(i_pvr),                      &
     &      color_params(i_pvr), cbar_params(i_pvr))
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                       'deallocate_cont_dat_pvr', i_pvr
        call deallocate_cont_dat_pvr(pvr_ctl_struct(i_pvr))
     end do
!
      call deallocate_pvr_file_header_ctl
!
      end subroutine s_set_pvr_control
!
!   --------------------------------------------------------------------
!
      subroutine read_control_pvr(i_pvr)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: i_pvr
!
      if(fname_pvr_ctl(i_pvr) .eq. 'NO_FILE') return
!
      call reset_pvr_control_flags(pvr_ctl_struct(i_pvr))
      if(my_rank .eq. 0) write(*,*) 'PVR control:', i_pvr,':  ',        &
     &                      trim( fname_pvr_ctl(i_pvr) )
!
      open(pvr_ctl_file_code, file=fname_pvr_ctl(i_pvr), status='old')
      call read_control_data_pvr(pvr_ctl_struct(i_pvr))
      close(pvr_ctl_file_code)
!
      end subroutine read_control_pvr
!
!  ---------------------------------------------------------------------
!
      end module set_pvr_control
