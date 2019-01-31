!radial_stack_4_cubed_sph.f90
!      module radial_stack_4_cubed_sph
!
!     Written by H. Matsui
!     Modified by H. Matsui on Oct., 2007
!
!!      subroutine radial_stack_quad                                    &
!!     &         (ifile, ifile_20, c_sphere, csph_mesh, iele_start)
!!      subroutine radial_stack_surf_q                                  &
!!     &         (ifile, ifile_20, c_sphere, csph_mesh, iele_start)
!!        type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!!        type(cubed_sph_mesh), intent(in) :: csph_mesh
!
      module radial_stack_4_cubed_sph
!
      use m_precision
!
      use t_cubed_sph_surf_mesh
      use t_cubed_sph_mesh
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine radial_stack_quad                                      &
     &         (ifile, ifile_20, c_sphere, csph_mesh, iele_start)
!
      integer(kind = kint), intent(in) :: ifile
      integer(kind = kint), intent(in) :: ifile_20
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
      type(cubed_sph_mesh), intent(in) :: csph_mesh
!
      integer(kind = kint), intent(inout) :: iele_start
!
      integer(kind = kint) :: k, iele0, iele
      integer(kind = kint) :: inod0, inod9, inod17
      integer(kind = kint) :: ie20(20)
!
!
      do k = 1, c_sphere%nele_shell
!
       do iele0 = 1, c_sphere%numele_sf
!
        iele = iele_start + c_sphere%numele_sf*(k-1) + iele0
        inod0 = c_sphere%numnod_cube + c_sphere%numnod_sf * (k-1)
!
        ie20(1) = inod0 + c_sphere%ie_sf20(iele0,1)
        ie20(2) = inod0 + c_sphere%ie_sf20(iele0,2)
        ie20(3) = inod0 + c_sphere%ie_sf20(iele0,3)
        ie20(4) = inod0 + c_sphere%ie_sf20(iele0,4)
        ie20(5) = inod0 + c_sphere%ie_sf20(iele0,1)                     &
     &           + c_sphere%numnod_sf
        ie20(6) = inod0 + c_sphere%ie_sf20(iele0,2)                     &
     &           + c_sphere%numnod_sf
        ie20(7) = inod0 + c_sphere%ie_sf20(iele0,3)                     &
     &           + c_sphere%numnod_sf
        ie20(8) = inod0 + c_sphere%ie_sf20(iele0,4)                     &
     &           + c_sphere%numnod_sf
!
        inod9 = csph_mesh%nnod_cb_sph + c_sphere%numedge_cube           &
     &         + (c_sphere%numnod_sf + c_sphere%numedge_sf)*(k-2)
!
        ie20( 9) = inod9 + c_sphere%ie_sf20(iele0,5)
        ie20(10) = inod9 + c_sphere%ie_sf20(iele0,6)
        ie20(11) = inod9 + c_sphere%ie_sf20(iele0,7)
        ie20(12) = inod9 + c_sphere%ie_sf20(iele0,8)
!
        ie20(13) = inod9 + c_sphere%ie_sf20(iele0,5)                    &
     &            + c_sphere%numnod_sf + c_sphere%numedge_sf
        ie20(14) = inod9 + c_sphere%ie_sf20(iele0,6)                    &
     &            + c_sphere%numnod_sf + c_sphere%numedge_sf
        ie20(15) = inod9 + c_sphere%ie_sf20(iele0,7)                    &
     &            + c_sphere%numnod_sf + c_sphere%numedge_sf
        ie20(16) = inod9 + c_sphere%ie_sf20(iele0,8)                    &
     &            + c_sphere%numnod_sf + c_sphere%numedge_sf
!
        inod17 = csph_mesh%nnod_cb_sph + c_sphere%numedge_cube          &
     &          + (c_sphere%numnod_sf + c_sphere%numedge_sf)*(k-1)
!
        ie20(17) = inod17 + c_sphere%ie_sf20(iele0,1)
        ie20(18) = inod17 + c_sphere%ie_sf20(iele0,2)
        ie20(19) = inod17 + c_sphere%ie_sf20(iele0,3)
        ie20(20) = inod17 + c_sphere%ie_sf20(iele0,4)
!
        write(ifile,    '(21i16)') iele, ie20(1:8)
        if (ifile_20 .gt. 0) then
          write(ifile_20 ,'(21i16)') iele, ie20(1:20)
        end if
!
        end do
      end do
!
      iele_start = iele_start                                           &
     &            + c_sphere%numele_sf * c_sphere%nele_shell
!
      end subroutine radial_stack_quad
!
!   --------------------------------------------------------------------
!
      subroutine radial_stack_surf_q                                    &
     &         (ifile, ifile_20, c_sphere, csph_mesh, iele_start)
!
      integer(kind = kint), intent(in) :: ifile
      integer(kind = kint), intent(in) :: ifile_20
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
      type(cubed_sph_mesh), intent(in) :: csph_mesh
!
      integer(kind = kint), intent(inout) :: iele_start
!
      integer(kind = kint) :: k, iele0, iele
      integer(kind = kint) :: inod0, inod9, inod17
      integer(kind = kint) :: ie20(20)
!
!
      do k = 1, c_sphere%nele_shell
       do iele0 = 1, c_sphere%ntot_edge_sf20
!
        iele = iele_start + c_sphere%ntot_edge_sf20*(k-1) + iele0
        inod0 = c_sphere%numnod_cube + c_sphere%numnod_sf * (k-1)
!
        ie20(1) = inod0 + c_sphere%iedge_sf20(iele0,3)
        ie20(2) = inod0 + c_sphere%iedge_sf20(iele0,1)
        ie20(3) = inod0 + c_sphere%iedge_sf20(iele0,1)                  &
     &                  + c_sphere%numnod_sf
        ie20(4) = inod0 + c_sphere%iedge_sf20(iele0,3)                  &
     &                  + c_sphere%numnod_sf
!
        inod9 = csph_mesh%nnod_cb_sph + c_sphere%numedge_cube           &
     &         + (c_sphere%numnod_sf + c_sphere%numedge_sf)*(k-2)
!
        ie20( 5) = inod9 + c_sphere%iedge_sf20(iele0,2)
!
        inod17 = csph_mesh%nnod_cb_sph + c_sphere%numedge_cube          &
     &          + (c_sphere%numnod_sf + c_sphere%numedge_sf)*(k-1)
        ie20( 7) = inod17 + c_sphere%iedge_sf20(iele0,2)
!
        inod17 = csph_mesh%nnod_cb_sph + c_sphere%numedge_cube          &
     &          + (c_sphere%numnod_sf + c_sphere%numedge_sf)*(k-1)
!
        ie20( 6) = inod17 + c_sphere%iedge_sf20(iele0,1)
        ie20( 8) = inod17 + c_sphere%iedge_sf20(iele0,3)
!
        write(ifile,    '(21i16)') iele, ie20(1:4)
        if (ifile_20 .gt. 0) then
          write(ifile_20 ,'(21i16)') iele, ie20(1:8)
        end if
!
        end do
      end do
!
      iele_start = iele_start                                           &
     &            + c_sphere%numele_sf * c_sphere%nele_shell
!
      end subroutine radial_stack_surf_q
!
!   --------------------------------------------------------------------
!
      end module radial_stack_4_cubed_sph
