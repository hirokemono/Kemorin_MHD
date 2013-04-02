!
!     module m_filter_data_4_plane
!
      module m_filter_data_4_plane
!
!     Written by Kemorin
!

      use m_precision
!
      implicit none
!
!
!
      integer (kind= kint), parameter :: filter_id = 17
      character(len=kchara) :: filtername
      integer (kind= kint) :: iwidth_1d, iwidth_2d, iwidth_3d
!
      character(len=kchara), dimension(:), allocatable :: filtertype_h
      character(len=kchara), dimension(:), allocatable :: filtertype_z
!
      integer (kind = kint), dimension(3) :: inod_width
!
      real (kind = kreal), dimension(:), allocatable :: width_f
      real (kind = kreal), dimension(:,:,:), allocatable :: mom_1d_o
      real (kind = kreal), dimension(:,:,:), allocatable :: mom_1d_z
      real (kind = kreal), dimension(:,:,:), allocatable :: dmom_1d_z
!
      real (kind = kreal), dimension(:,:,:), allocatable :: coef_4_filter
!
      real (kind = kreal), dimension(:,:,:), allocatable :: coef_nod_x
      real (kind = kreal), dimension(:,:,:), allocatable :: coef_nod_y
      real (kind = kreal), dimension(:,:,:,:), allocatable :: coef_nod_z
!
      real (kind = kreal), dimension(:), allocatable :: z_filter
      real (kind = kreal), dimension(:), allocatable :: delta_z
      real (kind = kreal), dimension(:), allocatable :: diff_deltaz
      real (kind = kreal), dimension(:), allocatable :: d2_deltaz
      real (kind = kreal), dimension(2) :: delta_h
      real (kind = kreal), dimension(2) :: diff_deltah
!
      real (kind = kreal), dimension(:), allocatable :: delta_z_e
      real (kind = kreal), dimension(:), allocatable :: diff_deltaz_e
      real (kind = kreal), dimension(:), allocatable :: d2_deltaz_e
!
      integer (kind = kint), dimension(:), allocatable :: nneib_h
      integer (kind = kint), dimension(:,:,:), allocatable :: nneib_z
      integer (kind = kint), dimension(:,:,:), allocatable :: ineib_z
!
!  ----------------------------------------------------------------------
!
      contains
!
!  ----------------------------------------------------------------------
!
       subroutine allocate_filter_4_plane
!
       use m_size_4_plane
       use m_size_of_cube
       use m_filtering_nod_4_cubmesh
!
       use m_filter_elength
!
       iwidth_1d = 2*ndepth+1
       iwidth_2d = iwidth_1d*iwidth_1d
       iwidth_3d = iwidth_2d*iwidth_1d
!
       allocate( nneib_h(iwidth_1d) )
       allocate( nneib_z(nz_all,2,nf_type) )
       allocate( ineib_z(nz_all,iwidth_1d,nf_type) )
!
       allocate( coef_4_filter(nz_all,iwidth_1d,nf_type) )
!
       allocate( coef_nod_x(iwidth_1d,0:2,nf_type) )
       allocate( coef_nod_y(iwidth_1d,0:2,nf_type) )
       allocate( coef_nod_z(nz_all,iwidth_1d,0:2,nf_type) )
!
       allocate( filtertype_h(nf_type) )
       allocate( filtertype_z(nf_type) )
!
       allocate( width_f(nf_type) )
       allocate( mom_1d_o(0:2,3,nf_type) )
       allocate( mom_1d_z(nz_all,0:2,nf_type) )
       allocate( dmom_1d_z(nz_all,0:2,nf_type) )
!
       allocate( z_filter(nz_all) )
       allocate( delta_z(nz_all) )
       allocate( diff_deltaz(nz_all) )
       allocate( d2_deltaz(nz_all) )
!
       allocate( delta_z_e(nz_all-1) )
       allocate( diff_deltaz_e(nz_all-1) )
       allocate( d2_deltaz_e(nz_all-1) )
!
       nneib_h = 0
       nneib_z = 0
       ineib_z = 0
!
       coef_nod_x = 0.0d0
       coef_nod_y = 0.0d0
       coef_nod_z = 0.0d0
!
       width_f = 0.0d0
       mom_1d_o = 0.0d0
       mom_1d_z = 0.0d0
       dmom_1d_z = 0.0d0
       coef_4_filter = 0.0d0
!
       z_filter = 0.0d0
       delta_z = 0.0d0
       diff_deltaz = 0.0d0
       d2_deltaz = 0.0d0
!
       delta_z_e = 0.0d0
       diff_deltaz_e = 0.0d0
       d2_deltaz_e = 0.0d0
!
       end subroutine allocate_filter_4_plane
!
!  ----------------------------------------------------------------------
!
      end module m_filter_data_4_plane
