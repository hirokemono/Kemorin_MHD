!count_shell_configration.f90
!      module count_shell_configration
!
!        programmed by H.Matsui on Apr., 2006
!
!
!!      subroutine count_cubed_shell_size                               &
!!     &         (csph_p, rprm_csph, c_sphere, csph_mesh, csph_grp)
!!      subroutine count_rectangle_shell_size                           &
!!     &         (csph_p, rprm_csph, c_sphere, csph_mesh, csph_grp)
!!      subroutine count_rectangle_shell_size(r1, c_sphere, csph_mesh)
!!        type(numref_cubed_sph), intent(inout) :: csph_p
!!        type(cubed_sph_radius), intent(inout) :: rprm_csph
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!!        type(cubed_sph_mesh), intent(inout) :: csph_mesh
!!        type(cubed_sph_group, intent(inout) :: csph_grp
!!      subroutine count_radial_layer_size                              &
!!     &         (nele_shell, rprm_csph, csph_grp)
!!
!!      subroutine count_center_cube_size(num_hemi,                     &
!!     &          nnod_cube, nele_cube, nedge_cube, nsurf_cube,         &
!!     &          nnod_cube20, nnod_sf, nele_sf, nedge_sf)
!!      subroutine count_center_rect_size(num_hemi, ncube_vertical,     &
!!     &          nnod_cube, nele_cube, nedge_cube, nsurf_cube,         &
!!     &          nnod_cube20, nnod_sf, nele_sf, nedge_sf)
!
      module count_shell_configration
!
      use m_precision
      use m_constants
      use t_cubed_sph_radius
      use t_cubed_sph_surf_mesh
      use t_cubed_sph_mesh
      use t_cubed_sph_grp_param
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
      subroutine count_cubed_shell_size                                 &
     &         (csph_p, rprm_csph, c_sphere, csph_mesh, csph_grp)
!
      use t_numref_cubed_sph
!
      type(numref_cubed_sph), intent(inout) :: csph_p
      type(cubed_sph_radius), intent(inout) :: rprm_csph
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
      type(cubed_sph_mesh), intent(inout) :: csph_mesh
      type(cubed_sph_group, intent(inout) :: csph_grp
!
!    count number of node & element
!      number of radius direction
!
      call count_radial_layer_size                                      &
     &   (c_sphere%nele_shell, rprm_csph, csph_grp)
!
!   numbers for center cube
!       (except for surface of cube for number of node)
!
      call count_center_cube_size                                       &
     &   (csph_p%num_hemi, c_sphere%numnod_cube,                        &
     &    c_sphere%numele_cube, c_sphere%numedge_cube,                  &
     &    c_sphere%numsurf_cube, c_sphere%numnod_cube20,                &
     &    c_sphere%numnod_sf, c_sphere%numele_sf, c_sphere%numedge_sf)
!
      call count_shell_numbers(rprm_csph%r_nod(1),                      &
     &    csph_p%cube_size, c_sphere, csph_mesh)
!
      end subroutine count_cubed_shell_size
!
!   --------------------------------------------------------------------
!
      subroutine count_rectangle_shell_size                             &
     &         (csph_p, rprm_csph, c_sphere, csph_mesh, csph_grp)
!
      use t_numref_cubed_sph
!
      type(numref_cubed_sph), intent(inout) :: csph_p
      type(cubed_sph_radius), intent(inout) :: rprm_csph
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
      type(cubed_sph_mesh), intent(inout) :: csph_mesh
      type(cubed_sph_group, intent(inout) :: csph_grp
!
!    count number of node & element
!      number of radius direction
!
      call count_radial_layer_size                                      &
     &   (c_sphere%nele_shell, rprm_csph, csph_grp)
!
!   numbers for center cube
!       (except for surface of cube for number of node)
!
      call count_center_rect_size                                       &
     &   (csph_p%num_hemi, csph_p%ncube_vertical, c_sphere%numnod_cube, &
     &    c_sphere%numele_cube, c_sphere%numedge_cube,                  &
     &    c_sphere%numsurf_cube, c_sphere%numnod_cube20,                &
     &    c_sphere%numnod_sf, c_sphere%numele_sf, c_sphere%numedge_sf)
!
      call count_shell_numbers(rprm_csph%r_nod(1),                      &
     &    csph_p%cube_size, c_sphere, csph_mesh)
!
      end subroutine count_rectangle_shell_size
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_radial_layer_size                                &
     &         (nele_shell, rprm_csph, csph_grp)
!
      integer(kind= kint), intent(inout) :: nele_shell
      type(cubed_sph_radius), intent(inout) :: rprm_csph
      type(cubed_sph_group, intent(inout) :: csph_grp
!
!    count number of node & element
!      number of radius direction
!
      nele_shell =   rprm_csph%n_shell - 1
!
      csph_grp%nr_icb =   csph_grp%nlayer_ICB - 1 
      csph_grp%nr_cmb =   csph_grp%nlayer_CMB - 1
      rprm_csph%nr_ocore = csph_grp%nlayer_CMB - csph_grp%nlayer_ICB
      rprm_csph%nr_exter = rprm_csph%n_shell - csph_grp%nlayer_CMB
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
      subroutine count_shell_numbers                                    &
     &         (r1, cube_size, c_sphere, csph_mesh)
!
      real(kind = kreal), intent(in) :: r1
!
      real(kind = kreal), intent(inout) :: cube_size
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
      type(cubed_sph_mesh), intent(inout) :: csph_mesh
!
!
      c_sphere%numnod_sf20                                              &
     &             = c_sphere%numnod_sf + c_sphere%numedge_sf
      c_sphere%numedge_sf20 = c_sphere%numedge_sf
      c_sphere%numele_sf20 =  c_sphere%numele_sf
!
      csph_mesh%nnod_cb_sph = c_sphere%numnod_cube + c_sphere%numnod_sf
      csph_mesh%nele_cb_sph =  c_sphere%numele_cube
      csph_mesh%nsurf_cb_sph = c_sphere%numsurf_cube
      csph_mesh%nedge_cb_sph = c_sphere%numedge_cube
      csph_mesh%nnod_cb_sph = csph_mesh%nnod_cb_sph                     &
     &             + c_sphere%numnod_sf * c_sphere%nele_shell
      csph_mesh%nele_cb_sph = csph_mesh%nele_cb_sph                     &
     &             + c_sphere%numele_sf * c_sphere%nele_shell
      csph_mesh%nedge_cb_sph = csph_mesh%nedge_cb_sph                   &
     &             + (c_sphere%numedge_sf + c_sphere%numnod_sf)         &
     &              * c_sphere%nele_shell
      csph_mesh%nsurf_cb_sph = csph_mesh%nsurf_cb_sph                   &
     &              + (c_sphere%numele_sf + c_sphere%numedge_sf)        &
     &              * c_sphere%nele_shell
      csph_mesh%numnod_20 = csph_mesh%nnod_cb_sph                       &
     &                     + csph_mesh%nedge_cb_sph
      csph_mesh%numele_20 = csph_mesh%nele_cb_sph
!
!  set center cube size
!
      cube_size = r1 * sqrt( one / three )
!
      end subroutine count_shell_numbers
!
!   --------------------------------------------------------------------
!
      end module count_shell_configration
