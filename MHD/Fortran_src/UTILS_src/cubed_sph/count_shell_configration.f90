!count_shell_configration.f90
!      module count_shell_configration
!
!        programmed by H.Matsui on Apr., 2006
!
!
!      subroutine count_cubed_shell_size
!      subroutine count_rectangle_shell_size
!
!      subroutine count_center_cube_size(num_hemi,                      &
!     &          nnod_cube, nele_cube, nedge_cube, nsurf_cube,          &
!     &          nnod_cube20, nnod_sf, nele_sf, nedge_sf)
!      subroutine count_center_rect_size(num_hemi, ncube_vertical,      &
!     &          nnod_cube, nele_cube, nedge_cube, nsurf_cube,          &
!     &          nnod_cube20, nnod_sf, nele_sf, nedge_sf)
!
      module count_shell_configration
!
      use m_precision
!
      implicit  none
!
      private :: count_shell_numbers
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine count_cubed_shell_size
!
      use m_numref_cubed_sph
      use m_cubed_sph_surf_mesh
!
!    count number of node & element
!      number of radius direction
!
      call count_radial_layer_size
!
!   numbers for center cube
!       (except for surface of cube for number of node)
!
      call count_center_cube_size(num_hemi,                             &
     &    numnod_cube, numele_cube, numedge_cube, numsurf_cube,   &
     &    numnod_cube20, c_sphere1%numnod_sf, c_sphere1%numele_sf,      &
     &    numedge_sf)
!
      call count_shell_numbers
!
      end subroutine count_cubed_shell_size
!
!   --------------------------------------------------------------------
!
      subroutine count_rectangle_shell_size
!
      use m_numref_cubed_sph
      use m_cubed_sph_surf_mesh
!
!    count number of node & element
!      number of radius direction
!
      call count_radial_layer_size
!
!   numbers for center cube
!       (except for surface of cube for number of node)
!
      call count_center_rect_size(num_hemi, ncube_vertical,             &
     &    numnod_cube, numele_cube, numedge_cube, numsurf_cube,   &
     &    numnod_cube20, c_sphere1%numnod_sf, c_sphere1%numele_sf,      &
     &    numedge_sf)
!
      call count_shell_numbers
!
      end subroutine count_rectangle_shell_size
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_radial_layer_size
!
      use m_cubed_sph_radius
      use m_cubed_sph_surf_mesh
      use m_cubed_sph_grp_param
!
!    count number of node & element
!      number of radius direction
!
      c_sphere1%nele_shell =   n_shell - 1
      nr_icb =   nlayer_ICB - 1 
      nr_cmb =   nlayer_CMB - 1
      nr_ocore = nlayer_CMB - nlayer_ICB
      nr_exter = n_shell - nlayer_CMB
!
      end subroutine count_radial_layer_size
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_center_cube_size(num_hemi,                       &
     &          nnod_cube, nele_cube, nedge_cube, nsurf_cube,           &
     &          nnod_cube20, nnod_sf, nele_sf, nedge_sf)
!
      integer(kind = kint), intent(in) :: num_hemi
      integer(kind = kint), intent(inout) :: nnod_cube, nele_cube
      integer(kind = kint), intent(inout) :: nedge_cube, nsurf_cube
      integer(kind = kint), intent(inout) :: nnod_cube20
      integer(kind = kint), intent(inout) :: nnod_sf, nele_sf
      integer(kind = kint), intent(inout) :: nedge_sf
!
!   numbers for center cube
!       (except for surface of cube for number of node)
!
      nnod_cube = (num_hemi-1)**3
      nele_cube = num_hemi**3
      nedge_cube = 3 * (num_hemi+1)**2 * num_hemi
      nsurf_cube = 3 * (num_hemi+1) * num_hemi**2
      nnod_cube20 = nnod_cube + 3 * num_hemi * (num_hemi-1)**2
!
      nnod_sf = 6*num_hemi**2 + 2
      nele_sf = 6*num_hemi**2
      nedge_sf = 12*num_hemi*num_hemi
!
      end subroutine count_center_cube_size
!
!   --------------------------------------------------------------------
!
      subroutine count_center_rect_size(num_hemi, ncube_vertical,       &
     &          nnod_cube, nele_cube, nedge_cube, nsurf_cube,           &
     &          nnod_cube20, nnod_sf, nele_sf, nedge_sf)
!
      integer(kind = kint), intent(in) :: num_hemi, ncube_vertical
      integer(kind = kint), intent(inout) :: nnod_cube, nele_cube
      integer(kind = kint), intent(inout) :: nedge_cube, nsurf_cube
      integer(kind = kint), intent(inout) :: nnod_cube20
      integer(kind = kint), intent(inout) :: nnod_sf, nele_sf
      integer(kind = kint), intent(inout) :: nedge_sf
!
!   numbers for center cube
!       (except for surface of cube for number of node)
!
      nnod_cube = (num_hemi-1)**2 * (ncube_vertical-1)
      nele_cube = num_hemi**2 * ncube_vertical
      nedge_cube = 2 * (num_hemi+1)*(ncube_vertical+1) * num_hemi       &
     &                 + (num_hemi+1)**2 * ncube_vertical
      nsurf_cube = 2 * (num_hemi+1) * num_hemi*ncube_vertical           &
     &                 + (ncube_vertical+1) * num_hemi**2
!
      nnod_cube20 = nnod_cube                                           &
     &               + 2 * num_hemi*ncube_vertical * (num_hemi-1)       &
     &                 + num_hemi**2 * (ncube_vertical-1)
!
      nnod_sf = 2*num_hemi**2 + 4*num_hemi*ncube_vertical + 2
      nele_sf =  2*num_hemi**2 + 4*num_hemi*ncube_vertical
      nedge_sf = 4*num_hemi*num_hemi + 8*num_hemi*ncube_vertical
!
      end subroutine count_center_rect_size
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_shell_numbers
!
      use m_constants
      use m_numref_cubed_sph
      use m_cubed_sph_mesh
      use m_cubed_sph_surf_mesh
      use m_cubed_sph_radius
!
!
      numnod_sf20 =  c_sphere1%numnod_sf + numedge_sf
      numedge_sf20 = numedge_sf
      numele_sf20 =  c_sphere1%numele_sf
!
      nnod_cb_sph = numnod_cube + c_sphere1%numnod_sf
      nele_cb_sph = numele_cube
      nsurf_cb_sph = numsurf_cube
      nedge_cb_sph = numedge_cube
      nnod_cb_sph = nnod_cb_sph                                         &
     &             + c_sphere1%numnod_sf * c_sphere1%nele_shell
      nele_cb_sph = nele_cb_sph                                         &
     &             + c_sphere1%numele_sf * c_sphere1%nele_shell
      nedge_cb_sph = nedge_cb_sph                                       &
     &             + (numedge_sf + c_sphere1%numnod_sf)                 &
     &              * c_sphere1%nele_shell
      nsurf_cb_sph = nsurf_cb_sph                                       &
     &              + (c_sphere1%numele_sf+numedge_sf)                  &
     &              * c_sphere1%nele_shell
      numnod_20 = nnod_cb_sph + nedge_cb_sph
      numele_20 = nele_cb_sph
!
!  set center cube size
!
      cube_size = r_nod(1) * sqrt( one / three )
!
      end subroutine count_shell_numbers
!
!   --------------------------------------------------------------------
!
      end module count_shell_configration
