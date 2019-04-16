!read_z_filter_info.f90
!     module read_z_filter_info
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!       subroutine read_filter_info(nf_type)
!
      module read_z_filter_info
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine read_filter_info(nf_type)
!
       use m_size_of_cube
       use m_cube_position
       use m_filter_data_4_plane
       use m_filtering_nod_4_cubmesh
       use m_cube_files_data
!
       use set_parallel_file_name
       use skip_comment_f
!
      integer (kind = kint), intent(in) :: nf_type
!
      integer (kind = kint) :: kf, ifil, ifil0, i, j, itmp
!
      character(len=255) :: tmpchara
      integer (kind = kint), dimension(3) :: numnod_f
      real (kind = kreal), dimension(3) :: size_f
      integer (kind = kint) :: i_grid
!
!
       do ifil = 1, nf_type
         ifil0 = ifil-1
!
         nb_name =    add_int_suffix(ifil0, z_filter_header)
         filtername = add_dat_extension(nb_name)
         write(*,*) 'filter filte name: ', filtername
         open (filter_id, file=filtername)
!
         call skip_comment(tmpchara,filter_id)
         read(tmpchara,*) (numnod_f(i), i=1, 3)
         if ( numnod_f(1) .ne. c_size1%nx_all                           &
     &   .or. numnod_f(2) .ne. c_size1%ny_all                           &
     &   .or. numnod_f(3) .ne. c_size1%nz_all ) then
          write(*,*) 'check spatial resolution'
          write(*,*) c_size1%nx_all, c_size1%ny_all, c_size1%nz_all
          write(*,*) numnod_f
          stop
         end if
!
         call skip_comment(tmpchara,filter_id)
         read(tmpchara,*) (size_f(i), i=1, 3)
         if ( size_f(1) .ne. c_size1%xsize                              &
     &   .or. size_f(2) .ne. c_size1%ysize                              &
     &   .or. size_f(3) .ne. c_size1%zsize ) then
          write(*,*) 'check size of simulation domain'
          stop
         end if
!
         call skip_comment(tmpchara,filter_id)
         read(tmpchara,*) i_grid
         if ( i_grid .ne. iradi ) then
          write(*,*) 'check grid spacing'
          write(*,*)  i_grid, iradi
          stop
         end if
!
         call skip_comment(tmpchara,filter_id)
         read(tmpchara,*) (inod_width(i), i=1, 3)
         if ( inod_width(1) .ne. iwidth_1d                              &
     &   .or. inod_width(2) .ne. iwidth_1d                              &
     &   .or. inod_width(3) .ne. iwidth_1d ) then
          write(*,*) 'check overlap width'
          stop
         end if
!
         call skip_comment(tmpchara,filter_id)
         read(tmpchara,*) filtertype_h(ifil)
         call skip_comment(tmpchara,filter_id)
         read(tmpchara,*) filtertype_z(ifil)
         call skip_comment(tmpchara,filter_id)
         read(tmpchara,*) width_f(ifil)
         call skip_comment(tmpchara,filter_id)
         read(tmpchara,*)  (mom_1d_o(i,1,ifil), i=0, 2)
         read(filter_id,*) (mom_1d_o(i,2,ifil), i=0, 2)
         read(filter_id,*) (mom_1d_o(i,3,ifil), i=0, 2)
!
         do i = 1, iwidth_1d
           call skip_comment(tmpchara,filter_id)
           read(tmpchara,*) itmp, (coef_nod_x(i,kf,ifil), kf=0,2)
         end do
!
         do i = 1, iwidth_1d
           call skip_comment(tmpchara,filter_id)
           read(tmpchara,*) itmp, (coef_nod_y(i,kf,ifil), kf=0,2)
         end do
!
         do i = 1, c_size1%nz_all
           call skip_comment(tmpchara,filter_id)
           read(tmpchara,*) itmp, z_filter(i), delta_z(i),              &
     &           diff_deltaz(i), d2_deltaz(i)
         end do
!
         do i = 1, c_size1%nz_all-1
           call skip_comment(tmpchara,filter_id)
           read(tmpchara,*) itmp, itmp, itmp, delta_z_e(i),             &
     &           diff_deltaz_e(i), d2_deltaz_e(i)
         end do
!
         do i = 1, c_size1%nz_all
           call skip_comment(tmpchara,filter_id)
           read(tmpchara,*) itmp, (nneib_z(i,j,ifil),j=1,2),            &
     &           (ineib_z(i,j,ifil), j=1,inod_width(3))
         end do
!
         do i = 1, c_size1%nz_all
           call skip_comment(tmpchara,filter_id)
           read(tmpchara,*) itmp,                                       &
     &           (coef_4_filter(i,j,ifil), j=1,iwidth_1d)
         end do
!
         do i = 1, c_size1%nz_all
           call skip_comment(tmpchara,filter_id)
           read(tmpchara,*) itmp,                                       &
     &           (mom_1d_z(i,kf,ifil), kf=0,2)
         end do
!
         do i = 1, c_size1%nz_all
           call skip_comment(tmpchara,filter_id)
           read(tmpchara,*) itmp,                                       &
     &           (dmom_1d_z(i,kf,ifil), kf=0,2)
         end do
!
         do kf = 0, 2
          read(filter_id,*) 
          do i = 1, c_size1%nz_all
           read(filter_id,*) itmp, itmp,                                &
     &           (coef_nod_z(i,j,kf,ifil), j=1,inod_width(3))
          end do
         end do
!
         close (filter_id)
!
       end do
!
       delta_h(1) = c_size1%xsize / dble(2*(c_size1%nx_all - 1))
       delta_h(2) = c_size1%ysize / dble(2*(c_size1%ny_all - 1))
       diff_deltah(1) = 0.0d0
       diff_deltah(2) = 0.0d0
       nneib_h(1) = (inod_width(1)-1)/2
       nneib_h(2) = (inod_width(2)-1)/2
!
      end subroutine read_filter_info
!
! ----------------------------------------------------------------------
!
      end module read_z_filter_info
