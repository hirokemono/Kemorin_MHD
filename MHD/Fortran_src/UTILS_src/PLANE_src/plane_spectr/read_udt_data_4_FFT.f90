!
!      module read_udt_data_4_FFT
!
!      Written by H. Matsui on Feb., 2007
!
!!      subroutine init_ucd_data_4_FFT(istep, ucd_param, t_IO, ucd)
!!      subroutine s_read_udt_data_4_FFT                                &
!!     &         (istep, ucd_param, mgd_mesh, plane_fft_wk, t_IO, ucd)
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(merged_mesh), intent(in) :: mgd_mesh
!!        type(time_data), intent(inout) :: t_IO
!!        type(ucd_data), intent(inout) :: ucd
!!      subroutine const_fields_4_FFT(field_ctl, plane_fft_wk)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!
      module read_udt_data_4_FFT
!
      use m_precision
!
      use m_constants
      use t_phys_data
      use copy_pick_udt_data_plane
!
      implicit none
!
      type(phys_data), private :: plane_phys
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_ucd_data_4_FFT(istep, ucd_param, t_IO, ucd)
!
      use m_constants
      use set_control_assemble
!
      use t_time_data
      use t_ucd_data
      use t_file_IO_parameter
!
      use set_list_4_FFT
      use ucd_IO_select
      use set_ucd_data_to_type
      use copy_pick_udt_data_plane
!
      integer (kind = kint), intent(in) :: istep
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
!
      ucd%nnod = ione
      call sel_read_udt_param(0, istep, ucd_param, t_IO, ucd)
!
      call alloc_phys_name_type_by_output(ucd, plane_phys)
      call deallocate_ucd_phys_data(ucd)
!
      end subroutine init_ucd_data_4_FFT
!
!  ---------------------------------------------------------------------
!
      subroutine s_read_udt_data_4_FFT                                  &
     &         (istep, ucd_param, mgd_mesh, plane_fft_wk, t_IO, ucd)
!
      use m_file_format_switch
      use t_spectr_4_ispack
      use t_mesh_data_4_merge
      use t_file_IO_parameter
!
      use t_time_data
      use t_ucd_data
!
      use set_list_4_FFT
!
      integer (kind = kint), intent(in) :: istep
      type(field_IO_params), intent(in) :: ucd_param
      type(merged_mesh), intent(in) :: mgd_mesh
      type(plane_spectr_by_ispack), intent(inout) :: plane_fft_wk
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
! ========================
! * PES loops 
! ========================
!
      call read_udt_data_4_plane_model                                  &
     &   (mgd_mesh%num_pe, istep, plane_fft_wk%num_spectr,              &
     &    plane_fft_wk%num_fft, plane_fft_wk%icomp_fft,                 &
     &    plane_fft_wk%ifield_fft, plane_fft_wk%phys_d,                 &
     &    mgd_mesh%merge_tbl%nnod_max, mgd_mesh%subdomain,              &
     &    ucd_param, t_IO, ucd)
!
       end subroutine s_read_udt_data_4_FFT
!
! -----------------------------------------------------------------------
!
      subroutine const_fields_4_FFT(field_ctl, plane_fft_wk)
!
      use t_spectr_4_ispack
      use t_read_control_arrays
      use skip_comment_f
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(plane_spectr_by_ispack), intent(inout) :: plane_fft_wk
!
      integer (kind = kint) :: i, j, ii, jj, icomp
!
!
      call count_fields_4_FFT(field_ctl, plane_fft_wk%num_fft)
      call alloc_spectr_name(plane_fft_wk)
      call set_fields_4_FFT(field_ctl, plane_fft_wk%num_fft,            &
     &    plane_fft_wk%fft_name, plane_fft_wk%fft_comp,                 &
     &    plane_fft_wk%ifield_fft, plane_fft_wk%icomp_fft)
!
      write(*,*) 'ifield_fft', plane_fft_wk%ifield_fft
      write(*,*) 'icomp_fft', plane_fft_wk%icomp_fft
!
      do ii = 1, plane_fft_wk%num_fft
       write(*,'(i3,1x,2a30)') ii,                                      &
     &    plane_fft_wk%fft_name(ii), plane_fft_wk%fft_comp(ii)
      end do
!
      end subroutine const_fields_4_FFT
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_fields_4_FFT(field_ctl, num_fft)
!
      use t_read_control_arrays
      use skip_comment_f
!
      type(ctl_array_c3), intent(in) :: field_ctl
      integer(kind = kint), intent(inout) :: num_fft
!
      integer (kind = kint) :: i, j, ii, jj, icomp
!
!
      num_fft = 0
      do i = 1, plane_phys%num_phys
        do j = 1, field_ctl%num
          if (cmp_no_case(field_ctl%c2_tbl(j), 'Viz_On')) then
            if (field_ctl%c1_tbl(j) .eq. plane_phys%phys_name(i)) then
              if ( plane_phys%num_component(i) .eq. 1) then
                num_fft = num_fft + 1
              else if ( plane_phys%num_component(i) .eq. 3) then
                num_fft = num_fft + 4
              else if ( plane_phys%num_component(i) .eq. 6) then
                num_fft = num_fft + 7
              end if
            end if
          end if
        end do
      end do
!
      end subroutine count_fields_4_FFT
!
! -----------------------------------------------------------------------
!
      subroutine set_fields_4_FFT(field_ctl, num_fft,                   &
     &          fft_name, fft_comp, ifield_fft, icomp_fft)
!
      use t_read_control_arrays
      use skip_comment_f
!
      type(ctl_array_c3), intent(in) :: field_ctl
      integer(kind = kint), intent(in) :: num_fft
!
      character(len=kchara), intent(inout) :: fft_name(num_fft)
      character(len=kchara), intent(inout) :: fft_comp(num_fft)
      integer(kind = kint), intent(inout) :: ifield_fft(num_fft)
      integer(kind = kint), intent(inout) :: icomp_fft(num_fft)
!
      integer (kind = kint) :: i, j, ii, jj, icomp
!
!
      icomp = 1
      ii = 1
      do i = 1, plane_phys%num_phys
        do j = 1, field_ctl%num
          if (cmp_no_case(field_ctl%c2_tbl(j), 'Viz_On')) then
            if (field_ctl%c1_tbl(j) .eq. plane_phys%phys_name(i)) then
!
              do jj = 1, plane_phys%num_component(i)
                fft_name(ii+jj-1) =   field_ctl%c1_tbl(j)
                ifield_fft(ii+jj-1) = icomp
                icomp_fft(ii+jj-1) = jj-1
              end do
!
              if ( plane_phys%num_component(i) .eq. 1) then
                fft_comp(ii) = 'scalar'
                ii = ii + 1
              else if ( plane_phys%num_component(i) .eq. 3) then
                fft_name(ii+3) = field_ctl%c1_tbl(j)
                ifield_fft(ii+3) = icomp
                fft_comp(ii  ) = 'x'
                fft_comp(ii+1) = 'y'
                fft_comp(ii+2) = 'z'
                fft_comp(ii+3) = 'vector'
                icomp_fft(ii+3) =-1
                ii = ii + 4
              else if ( plane_phys%num_component(i) .eq. 6) then
                fft_name(ii+6) = field_ctl%c1_tbl(j)
                ifield_fft(ii+6) = icomp
                fft_comp(ii  ) = 'xx'
                fft_comp(ii+1) = 'xy'
                fft_comp(ii+2) = 'xz'
                fft_comp(ii+3) = 'yy'
                fft_comp(ii+4) = 'yz'
                fft_comp(ii+5) = 'zz'
                fft_comp(ii+6) = 'tensor'
                icomp_fft(ii+6) =-2
                ii = ii + 7
              end if
!
            end if
          end if
!
        end do
        icomp = icomp + plane_phys%num_component(i)
      end do
!
      end subroutine set_fields_4_FFT
!
! -----------------------------------------------------------------------
!
      end module read_udt_data_4_FFT
