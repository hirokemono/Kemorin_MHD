!count_square_circle_config.f90
!      module count_square_circle_config
!
!        programmed by H.Matsui on Apr., 2006
!
!      subroutine count_square_circle_size
!
      module count_square_circle_config
!
      use m_precision
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
      subroutine count_square_circle_size
!
      use m_numref_cubed_sph
      use m_cubed_sph_surf_mesh
      use count_shell_configration
!
!    count number of node & element
!      number of radius direction
!
      call count_radial_layer_size
!
!   numbers for center cube
!       (except for surface of cube for number of node)
!
      call count_center_square_size(num_hemi,                           &
     &    numnod_cube, numele_cube, numedge_cube,                       &
     &    numnod_cube20, numnod_sf, numedge_sf)
!
      call count_curcle_numbers
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
      subroutine count_curcle_numbers
!
      use m_constants
      use m_numref_cubed_sph
      use m_cubed_sph_mesh
      use m_cubed_sph_surf_mesh
      use m_cubed_sph_radius
!
!
      numnod_sf20 =  numnod_sf + numedge_sf
      numedge_sf20 = numedge_sf
!
      numnod = numnod_cube + numnod_sf
      numele = numele_cube
      numedge = numedge_cube
      numnod = numnod + numnod_sf * nr
      numele = numele + numedge_sf * nr
      numedge = numedge + (numedge_sf+numnod_sf) * nr
      numnod_20 = numnod + numedge
      numele_20 = numele
!
!  set center cube size
!
      cube_size = r_nod(1) * sqrt( one / three )
!
      end subroutine count_curcle_numbers
!
!   --------------------------------------------------------------------
!
      end module count_square_circle_config
