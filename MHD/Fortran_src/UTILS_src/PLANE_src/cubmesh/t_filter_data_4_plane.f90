!
!     module t_filter_data_4_plane
!
!     Written by Kemorin
!
!!      subroutine read_z_filter_info                                   &
!!     &         (cube_p, c_size, nf_type, cube_fil)
!!         type(ctl_param_plane_mesh), intent(in) :: cube_p
!!         type(size_of_cube), intent(in) :: c_size
!!      subroutine alloc_filter_4_plane                                 &
!!     &         (ndepth, nz_all, nf_type, cube_fil)
!
      module t_filter_data_4_plane
!
      use m_precision
      use t_control_param_plane_mesh
!
      implicit none
!
      integer (kind= kint), parameter, private :: filter_id = 17
!
      type filter_data_4_plane
        integer (kind= kint) :: iwidth_1d
        integer (kind= kint) :: iwidth_2d
        integer (kind= kint) :: iwidth_3d
!
        character(len=kchara), allocatable :: filtertype_h(:)
        character(len=kchara), allocatable :: filtertype_z(:)
!
        integer (kind = kint) :: inod_width(3)
!
        real (kind = kreal), allocatable :: width_f(:)
        real (kind = kreal), allocatable :: mom_1d_o(:,:,:)
        real (kind = kreal), allocatable :: mom_1d_z(:,:,:)
        real (kind = kreal), allocatable :: dmom_1d_z(:,:,:)
!
        real (kind = kreal), allocatable :: coef_4_filter(:,:,:)
!
        real (kind = kreal), allocatable :: coef_nod_x(:,:,:)
        real (kind = kreal), allocatable :: coef_nod_y(:,:,:)
        real (kind = kreal), allocatable :: coef_nod_z(:,:,:,:)
!
        real (kind = kreal), allocatable :: z_filter(:)
        real (kind = kreal), allocatable :: delta_z(:)
        real (kind = kreal), allocatable :: diff_deltaz(:)
        real (kind = kreal), allocatable :: d2_deltaz(:)
        real (kind = kreal) :: delta_h(2)
        real (kind = kreal) :: diff_deltah(2)
!
        real (kind = kreal), allocatable :: delta_z_e(:)
        real (kind = kreal), allocatable :: diff_deltaz_e(:)
        real (kind = kreal), allocatable :: d2_deltaz_e(:)
!
        integer (kind = kint), allocatable :: nneib_h(:)
        integer (kind = kint), allocatable :: nneib_z(:,:,:)
        integer (kind = kint), allocatable :: ineib_z(:,:,:)
      end type filter_data_4_plane
!
!  ----------------------------------------------------------------------
!
      contains
!
!  ----------------------------------------------------------------------
!
       subroutine alloc_filter_4_plane                                  &
      &         (ndepth, nz_all, nf_type, cube_fil)
!
      integer(kind = kint), intent(in) :: ndepth, nz_all
      integer(kind = kint), intent(in) :: nf_type
      type(filter_data_4_plane), intent(inout) :: cube_fil
!
      integer(kind= kint) :: nw1
!
!
      cube_fil%iwidth_1d = 2*ndepth+1
      cube_fil%iwidth_2d = cube_fil%iwidth_1d**2
      cube_fil%iwidth_3d = cube_fil%iwidth_2d * cube_fil%iwidth_1d
!
      nw1 = cube_fil%iwidth_1d
      allocate(cube_fil%nneib_h(nw1) )
      allocate(cube_fil%nneib_z(nz_all,2,nf_type) )
      allocate(cube_fil%ineib_z(nz_all,nw1,nf_type) )
!
      allocate(cube_fil%coef_4_filter(nz_all,nw1,nf_type) )
!
      allocate(cube_fil%coef_nod_x(nw1,0:2,nf_type) )
      allocate(cube_fil%coef_nod_y(nw1,0:2,nf_type) )
      allocate(cube_fil%coef_nod_z(nz_all,nw1,0:2,nf_type) )
!
      allocate(cube_fil%filtertype_h(nf_type) )
      allocate(cube_fil%filtertype_z(nf_type) )
!
      allocate(cube_fil%width_f(nf_type) )
      allocate(cube_fil%mom_1d_o(0:2,3,nf_type) )
      allocate(cube_fil%mom_1d_z(nz_all,0:2,nf_type) )
      allocate(cube_fil%dmom_1d_z(nz_all,0:2,nf_type) )
!
      allocate(cube_fil%z_filter(nz_all) )
      allocate(cube_fil%delta_z(nz_all) )
      allocate(cube_fil%diff_deltaz(nz_all) )
      allocate(cube_fil%d2_deltaz(nz_all) )
!
      allocate(cube_fil%delta_z_e(nz_all-1) )
      allocate(cube_fil%diff_deltaz_e(nz_all-1) )
      allocate(cube_fil%d2_deltaz_e(nz_all-1) )
!
      cube_fil%nneib_h = 0
      cube_fil%nneib_z = 0
      cube_fil%ineib_z = 0
!
      cube_fil%coef_nod_x = 0.0d0
      cube_fil%coef_nod_y = 0.0d0
      cube_fil%coef_nod_z = 0.0d0
!
      cube_fil%width_f = 0.0d0
      cube_fil%mom_1d_o = 0.0d0
      cube_fil%mom_1d_z = 0.0d0
      cube_fil%dmom_1d_z = 0.0d0
      cube_fil%coef_4_filter = 0.0d0
!
      cube_fil%z_filter = 0.0d0
      cube_fil%delta_z = 0.0d0
      cube_fil%diff_deltaz = 0.0d0
      cube_fil%d2_deltaz = 0.0d0
!
      cube_fil%delta_z_e = 0.0d0
      cube_fil%diff_deltaz_e = 0.0d0
      cube_fil%d2_deltaz_e = 0.0d0
!
       end subroutine alloc_filter_4_plane
!
!  ----------------------------------------------------------------------
!
       subroutine dealloc_filter_4_plane(cube_fil)
!
      type(filter_data_4_plane), intent(inout) :: cube_fil
!
!
      deallocate(cube_fil%nneib_h)
      deallocate(cube_fil%nneib_z)
      deallocate(cube_fil%ineib_z)
!
      deallocate(cube_fil%coef_4_filter)
!
      deallocate(cube_fil%coef_nod_x)
      deallocate(cube_fil%coef_nod_y)
      deallocate(cube_fil%coef_nod_z)
!
      deallocate(cube_fil%filtertype_h)
      deallocate(cube_fil%filtertype_z)
!
      deallocate(cube_fil%width_f)
      deallocate(cube_fil%mom_1d_o)
      deallocate(cube_fil%mom_1d_z)
      deallocate(cube_fil%dmom_1d_z)
!
      deallocate(cube_fil%z_filter)
      deallocate(cube_fil%delta_z)
      deallocate(cube_fil%diff_deltaz)
      deallocate(cube_fil%d2_deltaz)
!
      deallocate(cube_fil%delta_z_e)
      deallocate(cube_fil%diff_deltaz_e)
      deallocate(cube_fil%d2_deltaz_e)
!
       end subroutine dealloc_filter_4_plane
!
!  ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_z_filter_info                                     &
     &         (cube_p, c_size, nf_type, cube_fil)
!
      use t_size_of_cube
!
      use set_parallel_file_name
      use skip_comment_f
!
      type(ctl_param_plane_mesh), intent(in) :: cube_p
      type(size_of_cube), intent(in) :: c_size
      integer(kind = kint), intent(in) :: nf_type
!
      type(filter_data_4_plane), intent(inout) :: cube_fil
!
      integer (kind = kint) :: kf, ifil, ifil0, i, j, itmp
!
      character(len=kchara) :: filtername, fname_tmp
      character(len=255) :: tmpchara
      integer (kind = kint), dimension(3) :: numnod_f
      real (kind = kreal), dimension(3) :: size_f
      integer (kind = kint) :: i_grid
!
!
       do ifil = 1, nf_type
         ifil0 = ifil-1
!
         fname_tmp =    add_int_suffix(ifil0, cube_p%z_filter_prefix)
         filtername = add_dat_extension(fname_tmp)
         write(*,*) 'filter filte name: ', filtername
         open (filter_id, file=filtername)
!
         call skip_comment(tmpchara,filter_id)
         read(tmpchara,*) (numnod_f(i), i=1, 3)
         if ( numnod_f(1) .ne. c_size%nx_all                            &
     &   .or. numnod_f(2) .ne. c_size%ny_all                            &
     &   .or. numnod_f(3) .ne. c_size%nz_all ) then
          write(*,*) 'check spatial resolution'
          write(*,*) c_size%nx_all, c_size%ny_all, c_size%nz_all
          write(*,*) numnod_f
          stop
         end if
!
         call skip_comment(tmpchara,filter_id)
         read(tmpchara,*) (size_f(i), i=1, 3)
         if ( size_f(1) .ne. c_size%xsize                               &
     &   .or. size_f(2) .ne. c_size%ysize                               &
     &   .or. size_f(3) .ne. c_size%zsize ) then
          write(*,*) 'check size of simulation domain'
          stop
         end if
!
         call skip_comment(tmpchara,filter_id)
         read(tmpchara,*) i_grid
         if ( i_grid .ne. cube_p%iflag_ztype) then
          write(*,*) 'check grid spacing'
          write(*,*)  i_grid, cube_p%iflag_ztype
          stop
         end if
!
         call skip_comment(tmpchara,filter_id)
         read(tmpchara,*) cube_fil%inod_width(1:3)
         if ( cube_fil%inod_width(1) .ne. cube_fil%iwidth_1d            &
     &   .or. cube_fil%inod_width(2) .ne. cube_fil%iwidth_1d            &
     &   .or. cube_fil%inod_width(3) .ne. cube_fil%iwidth_1d ) then
          write(*,*) 'check overlap width'
          stop
         end if
!
         call skip_comment(tmpchara,filter_id)
         read(tmpchara,*) cube_fil%filtertype_h(ifil)
         call skip_comment(tmpchara,filter_id)
         read(tmpchara,*) cube_fil%filtertype_z(ifil)
         call skip_comment(tmpchara,filter_id)
         read(tmpchara,*) cube_fil%width_f(ifil)
         call skip_comment(tmpchara,filter_id)
         read(tmpchara,*)  cube_fil%mom_1d_o(0:2,1,ifil)
         read(filter_id,*) cube_fil%mom_1d_o(0:2,2,ifil)
         read(filter_id,*) cube_fil%mom_1d_o(0:2,3,ifil)
!
         do i = 1, cube_fil%iwidth_1d
           call skip_comment(tmpchara,filter_id)
           read(tmpchara,*) itmp, cube_fil%coef_nod_x(i,0:2,ifil)
         end do
!
         do i = 1, cube_fil%iwidth_1d
           call skip_comment(tmpchara,filter_id)
           read(tmpchara,*) itmp, cube_fil%coef_nod_y(i,0:2,ifil)
         end do
!
         do i = 1, c_size%nz_all
           call skip_comment(tmpchara,filter_id)
           read(tmpchara,*) itmp,                                       &
     &           cube_fil%z_filter(i), cube_fil%delta_z(i),             &
     &           cube_fil%diff_deltaz(i), cube_fil%d2_deltaz(i)
         end do
!
         do i = 1, c_size%nz_all-1
           call skip_comment(tmpchara,filter_id)
           read(tmpchara,*) itmp, itmp, itmp, cube_fil%delta_z_e(i),    &
     &           cube_fil%diff_deltaz_e(i), cube_fil%d2_deltaz_e(i)
         end do
!
         do i = 1, c_size%nz_all
           call skip_comment(tmpchara,filter_id)
           read(tmpchara,*) itmp, cube_fil%nneib_z(i,1:2,ifil),         &
     &                cube_fil%ineib_z(i,1:cube_fil%inod_width(3),ifil)
         end do
!
         do i = 1, c_size%nz_all
           call skip_comment(tmpchara,filter_id)
           read(tmpchara,*) itmp,                                       &
     &           cube_fil%coef_4_filter(i,1:cube_fil%iwidth_1d,ifil)
         end do
!
         do i = 1, c_size%nz_all
           call skip_comment(tmpchara,filter_id)
           read(tmpchara,*) itmp, cube_fil%mom_1d_z(i,0:2,ifil)
         end do
!
         do i = 1, c_size%nz_all
           call skip_comment(tmpchara,filter_id)
           read(tmpchara,*) itmp, cube_fil%dmom_1d_z(i,0:2,ifil)
         end do
!
         do kf = 0, 2
          read(filter_id,*) 
          do i = 1, c_size%nz_all
           read(filter_id,*) itmp, itmp,                                &
     &         cube_fil%coef_nod_z(i,1:cube_fil%inod_width(3),kf,ifil)
          end do
         end do
!
         close (filter_id)
!
       end do
!
       cube_fil%delta_h(1) = c_size%xsize / dble(2*(c_size%nx_all - 1))
       cube_fil%delta_h(2) = c_size%ysize / dble(2*(c_size%ny_all - 1))
       cube_fil%diff_deltah(1) = 0.0d0
       cube_fil%diff_deltah(2) = 0.0d0
       cube_fil%nneib_h(1) = (cube_fil%inod_width(1)-1)/2
       cube_fil%nneib_h(2) = (cube_fil%inod_width(2)-1)/2
!
      end subroutine read_z_filter_info
!
! ----------------------------------------------------------------------
!
      end module t_filter_data_4_plane
