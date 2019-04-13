!sleeve_edge_corner_cube.f90
!     module sleeve_edge_corner_cube
!
!      Written by Kemorin
!
!!       subroutine set_sleeve_edge_xmin_ymin(sl_rng, kpe, knp,         &
!!     &          ioff_gl, koff, nd, inod)
!!       subroutine set_sleeve_edge_xmax_ymin(sl_rng, kpe, knp,         &
!!     &          ioff_gl, koff, nd, inod)
!!       subroutine set_sleeve_edge_xmax_ymax(sl_rng, kpe, knp,         &
!!     &          ioff_gl, koff, nd, inod)
!!       subroutine set_sleeve_edge_xmin_ymax(sl_rng, kpe, knp,         &
!!     &          ioff_gl, koff, nd, inod)
!!         type(slleve_range), intent(in) :: sl_rng
!
      module sleeve_edge_corner_cube
!
      use m_precision
      use m_constants
!
      use t_sleeve_cube
      use m_size_of_cube
      use m_size_4_plane
      use m_local_node_id_cube
      use m_cube_position
      use m_cube_files_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_edge_xmin_ymin(sl_rng, kpe, knp,            &
     &          ioff_gl, koff, nd, inod)
!
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl
      integer (kind = kint), intent(in) :: koff
      integer (kind = kint), intent(in) :: kpe, knp
      integer (kind = kint), intent(in) :: nd
      integer (kind = kint), intent(inout) :: inod
!
      integer (kind = kint) :: node_id_gl
      integer (kind = kint) :: i, j, k
      integer (kind = kint) ::is0, js0, ks0
      integer (kind = kint) ::ie0, je0, ke0
      real (kind = kreal) :: x, y, z
!
       is0 = 1
       ie0 = ndepth
       js0 = 1
       je0 = ndepth
       ks0 = sl_rng%ks
       ke0 = sl_rng%ke
       if ( nd.eq.3 .and. knp.gt.0) ke0 = ke0-1
       if ( nd.eq.3 .and. kpe.eq.ndz .and. knp.eq.0 ) ke0 = ke0-1
!
       do k = ks0, ke0
        do j = js0, je0
         do i = is0, ie0

          inod = inod + 1

          edge_id_lc(i,j,k,nd) =  inod
          node_id_gl = ioff_gl + i + (j-1) * ndepth                     &
     &                + (koff+k-1) * ndepth * ndepth

          if (nd .eq. 1) then
           x = xmin + ( dble(i)-half )*xsize / dble(nx_all)
           y = ymin + dble(j-1) *      ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 2) then
           x = xmin + dble(i-1) *      xsize / dble(nx_all)
           y = ymin + ( dble(j)-half )*ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 3) then
           x = xmin + dble(i-1) *      xsize / dble(nx_all)
           y = ymin + dble(j-1) *      ysize / dble(ny_all)
           z = zz_edge(koff+k)
          end if
!
           write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z
!
         end do
        end do
       end do
!
       end subroutine set_sleeve_edge_xmin_ymin
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_edge_xmax_ymin(sl_rng, kpe, knp,            &
     &          ioff_gl, koff, nd, inod)
!
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl
      integer (kind = kint), intent(in) :: koff
      integer (kind = kint), intent(in) :: kpe, knp
      integer (kind = kint), intent(in) :: nd
      integer (kind = kint), intent(inout) :: inod
!
      integer (kind = kint) :: node_id_gl
      integer (kind = kint) :: i, j, k
      integer (kind = kint) :: is0, js0, ks0
      integer (kind = kint) :: ie0, je0, ke0
      real (kind = kreal) :: x, y, z
!
       is0 = 1
       ie0 = ndepth
       js0 = 1
       je0 = ndepth
       ks0 = sl_rng%ks
       ke0 = sl_rng%ke
       if ( nd.eq.1 ) ie0 = ie0 - 1
       if ( nd.eq.3 .and. knp.gt.0) ke0 = ke0-1
       if ( nd.eq.3 .and. kpe.eq.ndz .and. knp.eq.0 ) ke0 = ke0-1
!
       do k = ks0, ke0
        do j = js0, je0
         do i = is0, ie0

          inod = inod + 1

          edge_id_lc(nxi+ndepth+i,j,k,nd) =  inod
          node_id_gl = ioff_gl + i + (j-1) * (ie0 - is0 + 1)            &
     &                + (koff+k-1) * (ie0 - is0 + 1) * ndepth

          if (nd .eq. 1) then
           x = xmax + ( dble(i+ndepth)-half )*xsize / dble(nx_all)
           y = ymin + dble(j-1) *             ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 2) then
           x = xmax + ( dble(i+ndepth-1) )*xsize / dble(nx_all)
           y = ymin + ( dble(j)-half )*ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 3) then
           x = xmax + ( dble(i+ndepth-1) )*xsize / dble(nx_all)
           y = ymin + dble(j-1) *      ysize / dble(ny_all)
           z = zz_edge(koff+k)
          end if

          write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z

         end do
        end do
       end do
!
       end subroutine set_sleeve_edge_xmax_ymin
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_edge_xmax_ymax(sl_rng, kpe, knp,            &
     &          ioff_gl, koff, nd, inod)
!
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl
      integer (kind = kint), intent(in) :: koff
      integer (kind = kint), intent(in) :: kpe, knp
      integer (kind = kint), intent(in) :: nd
      integer (kind = kint), intent(inout) :: inod
!
      integer (kind = kint) :: node_id_gl
      integer (kind = kint) :: i, j, k
      integer (kind = kint) :: is0, js0, ks0
      integer (kind = kint) :: ie0, je0, ke0
      real (kind = kreal) :: x, y, z
!
       is0 = 1
       ie0 = ndepth
       js0 = 1
       je0 = ndepth
       ks0 = sl_rng%ks
       ke0 = sl_rng%ke
       if ( nd.eq.1 ) ie0 = ie0 - 1
       if ( nd.eq.2 ) je0 = je0 - 1
       if ( nd.eq.3 .and. knp.gt.0) ke0 = ke0-1
       if ( nd.eq.3 .and. kpe.eq.ndz .and. knp.eq.0 ) ke0 = ke0-1
!
       do k = ks0, ke0
        do j = js0, je0
         do i = is0, ie0

          inod = inod + 1

          edge_id_lc(nxi+ndepth+i,nyi+ndepth+j,k,nd) =  inod
          node_id_gl = ioff_gl + i + (j-1) * (ie0 - is0 + 1)            &
     &                + (koff+k-1) * (ie0 - is0 + 1)                    &
     &                             * (je0 - js0 + 1)

          if (nd .eq. 1) then
           x = xmax + ( dble(i+ndepth)-half ) * xsize / dble(nx_all)
           y = ymax + dble(ndepth+j-1) *        ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 2) then
           x = xmax + dble(i+ndepth-1) *        xsize / dble(nx_all)
           y = ymax + ( dble(ndepth+j)-half ) * ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 3) then
           x = xmax + dble(i+ndepth-1) *        xsize / dble(nx_all)
           y = ymax + dble(ndepth+j-1) *        ysize / dble(ny_all)
           z = zz_edge(koff+k)
          end if

           write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z

         end do
        end do
       end do
!
       end subroutine set_sleeve_edge_xmax_ymax
!
!  ---------------------------------------------------------------------
!
      subroutine set_sleeve_edge_xmin_ymax(sl_rng, kpe, knp,            &
     &          ioff_gl, koff, nd, inod)
!
      type(slleve_range), intent(in) :: sl_rng
      integer (kind = kint), intent(in) :: ioff_gl
      integer (kind = kint), intent(in) :: koff
      integer (kind = kint), intent(in) :: kpe, knp
      integer (kind = kint), intent(in) :: nd
      integer (kind = kint), intent(inout) :: inod
!
      integer (kind = kint) :: node_id_gl
      integer (kind = kint) :: i, j, k
      integer (kind = kint) :: is0, js0, ks0
      integer (kind = kint) :: ie0, je0, ke0
      real (kind = kreal) :: x, y, z
!
      is0 = 1
      ie0 = ndepth
      js0 = 1
      je0 = ndepth
      ks0 = sl_rng%ks
      ke0 = sl_rng%ke
      if ( nd.eq.2 ) je0 = je0 - 1
      if ( nd.eq.3 .and. knp.gt.0) ke0 = ke0-1
      if ( nd.eq.3 .and. kpe.eq.ndz .and. knp.eq.0 ) ke0 = ke0-1
!
      do k = ks0, ke0
        do j = js0, je0
         do i = is0, ie0

          inod = inod + 1

          edge_id_lc(i,nyi+ndepth+j,k,nd) =  inod
          node_id_gl = ioff_gl + i + (j-1) * ndepth                     &
     &                + (koff+k-1) * ndepth * (je0 - js0 + 1)

          if (nd .eq. 1) then
           x = xmin + ( dble(i)-half ) *        xsize / dble(nx_all)
           y = ymax + dble(ndepth+j-1) *        ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 2) then
           x = xmin + dble(i-1) *               xsize / dble(nx_all)
           y = ymax + ( dble(ndepth+j)-half ) * ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 3) then
           x = xmin + dble(i-1) *               xsize / dble(nx_all)
           y = ymax + dble(ndepth+j-1) *        ysize / dble(ny_all)
           z = zz_edge(koff+k)
          end if

          write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z

         end do
        end do
      end do
!
      end subroutine set_sleeve_edge_xmin_ymax
!
!  ---------------------------------------------------------------------
!
      end module sleeve_edge_corner_cube
