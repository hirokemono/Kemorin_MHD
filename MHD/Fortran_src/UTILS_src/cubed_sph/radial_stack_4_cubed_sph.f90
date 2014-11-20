!radial_stack_4_cubed_sph.f90
!      module radial_stack_4_cubed_sph
!
!     Written by H. Matsui
!     Modified by H. Matsui on Oct., 2007
!
!      subroutine radial_stack_quad(iele_start, ifile, ifile_20)
!
      module radial_stack_4_cubed_sph
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine radial_stack_quad(iele_start, ifile, ifile_20)
!
!
      use m_cubed_sph_mesh
      use m_cubed_sph_surf_mesh
      use m_cubed_sph_radius
!
      integer(kind = kint), intent(in) :: ifile
      integer(kind = kint), intent(in) :: ifile_20
!
      integer(kind = kint), intent(inout) :: iele_start
!
      integer(kind = kint) :: k, iele0, iele
      integer(kind = kint) :: inod0, inod9, inod17
!
!
      do k = 1, nr
!
       do iele0 = 1, numele_sf
!
        iele = iele_start + numele_sf*(k-1) + iele0
        inod0 = numnod_cube + numnod_sf*(k-1)
!
        ie20(1) = inod0 + ie_sf20(iele0,1)
        ie20(2) = inod0 + ie_sf20(iele0,2)
        ie20(3) = inod0 + ie_sf20(iele0,3)
        ie20(4) = inod0 + ie_sf20(iele0,4)
        ie20(5) = inod0 + ie_sf20(iele0,1) + numnod_sf
        ie20(6) = inod0 + ie_sf20(iele0,2) + numnod_sf
        ie20(7) = inod0 + ie_sf20(iele0,3) + numnod_sf
        ie20(8) = inod0 + ie_sf20(iele0,4) + numnod_sf
!
        inod9 = numnod + numedge_cube + (numnod_sf + numedge_sf)*(k-2)
!
        ie20( 9) = inod9 + ie_sf20(iele0,5)
        ie20(10) = inod9 + ie_sf20(iele0,6)
        ie20(11) = inod9 + ie_sf20(iele0,7)
        ie20(12) = inod9 + ie_sf20(iele0,8)
!
        ie20(13) = inod9 + ie_sf20(iele0,5) + numnod_sf + numedge_sf
        ie20(14) = inod9 + ie_sf20(iele0,6) + numnod_sf + numedge_sf
        ie20(15) = inod9 + ie_sf20(iele0,7) + numnod_sf + numedge_sf
        ie20(16) = inod9 + ie_sf20(iele0,8) + numnod_sf + numedge_sf
!
        inod17 = numnod + numedge_cube + (numnod_sf + numedge_sf)*(k-1)
!
        ie20(17) = inod17 + ie_sf20(iele0,1)
        ie20(18) = inod17 + ie_sf20(iele0,2)
        ie20(19) = inod17 + ie_sf20(iele0,3)
        ie20(20) = inod17 + ie_sf20(iele0,4)
!
        write(ifile,    '(21i16)') iele, ie20(1:8)
        if (ifile_20 .gt. 0) then
          write(ifile_20 ,'(21i16)') iele, ie20(1:20)
        end if
!
        end do
      end do
!
      iele_start = iele_start + numele_sf*nr
!
      end subroutine radial_stack_quad
!
!   --------------------------------------------------------------------
!
      subroutine radial_stack_surf_q(iele_start, ifile, ifile_20)
!
!
      use m_cubed_sph_mesh
      use m_cubed_sph_surf_mesh
      use m_cubed_sph_radius
!
      integer(kind = kint), intent(in) :: ifile
      integer(kind = kint), intent(in) :: ifile_20
!
      integer(kind = kint), intent(inout) :: iele_start
!
      integer(kind = kint) :: k, iele0, iele
      integer(kind = kint) :: inod0, inod9, inod17
!
!
      do k = 1, nr
       do iele0 = 1, ntot_edge_sf20
!
        iele = iele_start + ntot_edge_sf20*(k-1) + iele0
        inod0 = numnod_cube + numnod_sf*(k-1)
!
        ie20(1) = inod0 + iedge_sf20(iele0,3)
        ie20(2) = inod0 + iedge_sf20(iele0,1)
        ie20(3) = inod0 + iedge_sf20(iele0,1) + numnod_sf
        ie20(4) = inod0 + iedge_sf20(iele0,3) + numnod_sf
!
        inod9 = numnod + numedge_cube + (numnod_sf + numedge_sf)*(k-2)
!
        ie20( 5) = inod9 + iedge_sf20(iele0,2)
!
        inod17 = numnod + numedge_cube + (numnod_sf + numedge_sf)*(k-1)
        ie20( 7) = inod17 + iedge_sf20(iele0,2)
!
        inod17 = numnod + numedge_cube + (numnod_sf + numedge_sf)*(k-1)
!
        ie20( 6) = inod17 + iedge_sf20(iele0,1)
        ie20( 8) = inod17 + iedge_sf20(iele0,3)
!
        write(ifile,    '(21i16)') iele, ie20(1:4)
        if (ifile_20 .gt. 0) then
          write(ifile_20 ,'(21i16)') iele, ie20(1:8)
        end if
!
        end do
      end do
!
      iele_start = iele_start + numele_sf*nr
!
      end subroutine radial_stack_surf_q
!
!   --------------------------------------------------------------------
!
      end module radial_stack_4_cubed_sph
