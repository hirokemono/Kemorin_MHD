!
!      module read_udt_data_4_FFT
!
!      Written by H. Matsui on Feb., 2007
!
!      subroutine init_ucd_data_4_FFT(istep)
!      subroutine s_read_udt_data_4_FFT(istep)
!      subroutine set_fields_4_FFT
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
      type(phys_data), private :: plane_ucd
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_ucd_data_4_FFT(istep)
!
      use m_constants
      use m_control_param_merge
      use set_list_4_FFT
      use ucd_IO_select
      use set_ucd_data_to_type
      use copy_pick_udt_data_plane
!
       integer (kind = kint), intent(in) :: istep
!
!
      nnod_ucd = ione
      call sel_read_udt_param(izero, istep)
!
      call alloc_phys_name_type_by_output(plane_ucd)
      call deallocate_ucd_phys_data
!
      end subroutine init_ucd_data_4_FFT
!
!  ---------------------------------------------------------------------
!
       subroutine s_read_udt_data_4_FFT(istep)
!
       use m_geometry_data_4_merge
       use m_spectr_4_ispack
       use m_file_format_switch
       use set_list_4_FFT
       use ucd_IO_select
!
       integer (kind = kint), intent(in) :: istep
!
       integer (kind = kint) :: ip, my_rank
!
! ========================
! * PES loops 
! ========================
!
      nnod_ucd = merge_tbl%nnod_max
      call allocate_ucd_phys_data
      do ip =1, num_pe
!
        my_rank = ip - 1
        nnod_ucd = subdomain(ip)%node%numnod
        call sel_read_udt_file(my_rank, istep)
!
        call copy_and_pick_udt_data_merge                               &
     &    (subdomain(ip)%node%numnod, subdomain(ip)%node%internal_node, &
     &     num_spectr, subdomain(ip)%node%inod_global,                  &
     &     num_fft, icomp_fft, ifield_fft, phys_d)
      end do
      call deallocate_ucd_phys_data
!
       end subroutine s_read_udt_data_4_FFT
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_fields_4_FFT
!
      use m_spectr_4_ispack
      use m_ctl_data_4_fields
!
      integer (kind = kint) :: i, j, ii, jj, icomp
!
!
      num_fft = 0
      do i = 1, plane_ucd%num_phys
        do j = 1, num_nod_phys_ctl
          if ( visualize_ctl(j) .eq. 'Viz_On' ) then
            if (phys_nod_name_ctl(j) .eq. plane_ucd%phys_name(i) ) then
              if ( plane_ucd%num_component(i) .eq. 1) then
                num_fft = num_fft + 1
              else if ( plane_ucd%num_component(i) .eq. 3) then
                num_fft = num_fft + 4
              else if ( plane_ucd%num_component(i) .eq. 6) then
                num_fft = num_fft + 7
              end if
            end if
          end if
        end do
      end do
!
      call allocate_spectr_name
!
      icomp = 1
      ii = 1
      do i = 1, plane_ucd%num_phys
        do j = 1, num_nod_phys_ctl
!
          if ( visualize_ctl(j) .eq. 'Viz_On' ) then
            if (phys_nod_name_ctl(j) .eq. plane_ucd%phys_name(i) ) then
!
              do jj = 1, plane_ucd%num_component(i)
                fft_name(ii+jj-1) =   phys_nod_name_ctl(j)
                ifield_fft(ii+jj-1) = icomp
                icomp_fft(ii+jj-1) = jj-1
              end do
!
              if ( plane_ucd%num_component(i) .eq. 1) then
                fft_comp(ii) = 'scalar'
                ii = ii + 1
              else if ( plane_ucd%num_component(i) .eq. 3) then
                fft_name(ii+3) = phys_nod_name_ctl(j)
                ifield_fft(ii+3) = icomp
                fft_comp(ii  ) = 'x'
                fft_comp(ii+1) = 'y'
                fft_comp(ii+2) = 'z'
                fft_comp(ii+3) = 'vector'
                icomp_fft(ii+3) =-1
                ii = ii + 4
              else if ( plane_ucd%num_component(i) .eq. 6) then
                fft_name(ii+6) = phys_nod_name_ctl(j)
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
        icomp = icomp + plane_ucd%num_component(i)
      end do
!
      write(*,*) 'ifield_fft', ifield_fft
      write(*,*) 'icomp_fft', icomp_fft
!
      do ii = 1, num_fft
       write(*,'(i3,1x,2a30)') ii, fft_name(ii), fft_comp(ii)
      end do
!
      end subroutine set_fields_4_FFT
!
! -----------------------------------------------------------------------
!
      end module read_udt_data_4_FFT
