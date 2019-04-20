!count_square_circle_config.f90
!      module count_square_circle_config
!
!        programmed by H.Matsui on Apr., 2006
!
!!      subroutine count_square_circle_size                             &
!!     &         (csph_p, rprm_csph, csph_grp, c_sphere, csph_mesh)
!!        type(numref_cubed_sph), intent(inout) :: csph_p
!!        type(cubed_sph_radius), intent(inout) :: rprm_csph
!!        type(cubed_sph_group), intent(inout) :: csph_grp
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!!        type(cubed_sph_mesh), intent(inout) :: csph_mesh
!
      module count_square_circle_config
!
      use m_precision
      use t_cubed_sph_radius
      use t_cubed_sph_surf_mesh
      use t_cubed_sph_mesh
!
      implicit  none
!
      private :: count_center_square_size, count_curcle_numbers
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine count_square_circle_size                               &
     &         (csph_p, rprm_csph, csph_grp, c_sphere, csph_mesh)
!
      use t_cubed_sph_grp_param
      use t_numref_cubed_sph
      use count_shell_configration
!
      type(numref_cubed_sph), intent(inout) :: csph_p
      type(cubed_sph_radius), intent(inout) :: rprm_csph
      type(cubed_sph_group), intent(inout) :: csph_grp
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
      type(cubed_sph_mesh), intent(inout) :: csph_mesh
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
      call count_center_square_size                                     &
     &   (csph_p%num_hemi, c_sphere%numnod_cube, c_sphere%numele_cube,  &
     &    c_sphere%numedge_cube, c_sphere%numnod_cube20,                &
     &    c_sphere%numnod_sf, c_sphere%numedge_sf)
!
      call count_curcle_numbers                                         &
     &   (rprm_csph%r_nod(1), csph_p%cube_size, c_sphere, csph_mesh)
!
      end subroutine count_square_circle_size
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_center_square_size(num_hemi,                     &
     &          nnod_cube, nele_cube, nedge_cube,                       &
     &          nnod_cube20, nnod_sf, nedge_sf)
!
      integer(kind = kint), intent(in) :: num_hemi
      integer(kind = kint), intent(inout) :: nnod_cube, nele_cube
      integer(kind = kint), intent(inout) :: nedge_cube
      integer(kind = kint), intent(inout) :: nnod_cube20
      integer(kind = kint), intent(inout) :: nnod_sf
      integer(kind = kint), intent(inout) :: nedge_sf
!
!   numbers for center cube
!       (except for surface of cube for number of node)
!
      nnod_cube = (num_hemi-1)**2
      nele_cube = num_hemi**2
      nedge_cube = 2 * (num_hemi+1) * num_hemi
      nnod_cube20 = nnod_cube + 2 * num_hemi * (num_hemi-1)
!
      nnod_sf =  4*num_hemi
      nedge_sf = 4*num_hemi
!
      end subroutine count_center_square_size
!
!   --------------------------------------------------------------------
!
      subroutine count_curcle_numbers                                   &
     &         (r1, cube_size, c_sphere, csph_mesh)
!
      use m_constants
!
      real(kind = kreal), intent(in) :: r1
      real(kind = kreal), intent(inout) :: cube_size
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
      type(cubed_sph_mesh), intent(inout) :: csph_mesh
!
!
      c_sphere%numnod_sf20                                              &
     &      = c_sphere%numnod_sf + c_sphere%numedge_sf
      c_sphere%numedge_sf20 = c_sphere%numedge_sf
!
      csph_mesh%nnod_cb_sph =  c_sphere%numnod_cube + c_sphere%numnod_sf
      csph_mesh%nele_cb_sph =  c_sphere%numele_cube
      csph_mesh%nedge_cb_sph = c_sphere%numedge_cube
      csph_mesh%nnod_cb_sph = csph_mesh%nnod_cb_sph                     &
     &             + c_sphere%numnod_sf * c_sphere%nele_shell
      csph_mesh%nele_cb_sph = csph_mesh%nele_cb_sph                     &
     &             + c_sphere%numedge_sf * c_sphere%nele_shell
      csph_mesh%nedge_cb_sph = csph_mesh%nedge_cb_sph                   &
     &              + (c_sphere%numedge_sf + c_sphere%numnod_sf)        &
     &              * c_sphere%nele_shell
      csph_mesh%numnod_20 = csph_mesh%nnod_cb_sph                       &
     &                     + csph_mesh%nedge_cb_sph
      csph_mesh%numele_20 = csph_mesh%nele_cb_sph
!
!  set center cube size
!
      cube_size = r1 * sqrt( one / three )
!
      end subroutine count_curcle_numbers
!
!   --------------------------------------------------------------------
!
      end module count_square_circle_config
