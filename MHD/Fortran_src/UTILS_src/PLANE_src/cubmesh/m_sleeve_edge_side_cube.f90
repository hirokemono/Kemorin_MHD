!m_sleeve_edge_side_cube.f90
!     module m_sleeve_edge_side_cube
!
      module m_sleeve_edge_side_cube
!
!      Written by Kemorin
!
      use m_precision
!
      use m_size_of_cube
      use m_size_4_plane
      use m_offset_size_cube
      use m_sleeve_cube
      use m_cube_position
      use m_local_node_id_cube
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
!      subroutine set_sleeve_edge_xmin
!
       subroutine set_sleeve_edge_xmin(kpe, jnp, knp,                   &
     &          inod, ioff_gl, nd)
!
       integer (kind = kint) :: ioff_gl
       integer (kind = kint) :: kpe, jnp, knp
       integer (kind = kint) :: inod
       integer (kind = kint) :: nd

!
       integer (kind = kint) :: node_id_gl
       integer (kind = kint) :: i, j, k
       integer (kind = kint) ::je1, ke1
       real (kind = kreal) :: x, y, z
       real (kind = kreal), parameter :: half = 0.5d0
!
!
       je1 = je
       ke1 = ke
       if ( nd.eq.2 .and. jnp.gt.0 ) je1 = je1 - 1
       if ( nd.eq.3 .and. knp.gt.0) ke1 = ke1-1
       if ( nd.eq.3 .and. kpe.eq.ndz .and. knp.eq.0 ) ke1 = ke1-1

       do k = ks, ke1
        do j = js, je1
         do i = is, ie

          inod = inod + 1

          edge_id_lc(i,j,k,nd) =  inod
          node_id_gl = ioff_gl + i + (joff+j)*(ie-is+1)                 &
     &               + (koff+k-1)*(ie-is+1)*ny_all

          if (nd .eq. 1) then
           x = xmin + ( dble(i)-half )*xsize / dble(nx_all)
           y = yoff + dble(j-1) *      ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 2) then
           x = xmin + dble(i-1) *      xsize / dble(nx_all)
           y = yoff + ( dble(j)-half )*ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 3) then
           x = xmin + dble(i-1) *      xsize / dble(nx_all)
           y = yoff + dble(j-1) *      ysize / dble(ny_all)
           z = zz_edge(koff+k)
          end if

           write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z
!
         end do
        end do
       end do
!
       end subroutine set_sleeve_edge_xmin
!
!  ---------------------------------------------------------------------
!
!      subroutine set_sleeve_edge_xmax
!
       subroutine set_sleeve_edge_xmax(kpe, jnp, knp,                   &
     &          inod, ioff_gl, nd)

!
!      Written by Kemorin
!
       integer (kind = kint) :: kpe, jnp, knp
       integer (kind = kint) :: ioff_gl
       integer (kind = kint) :: inod
       integer (kind = kint) :: nd
!
       integer (kind = kint) :: node_id_gl
       integer (kind = kint) ::ie1, je1, ke1
       integer (kind = kint) :: i, j, k
       real (kind = kreal) :: x, y, z
       real (kind = kreal), parameter :: half = 0.5d0
!
!
       ie1 = ie
       je1 = je
       ke1 = ke
       if ( nd.eq.1 ) ie1 = ie1 - 1
       if ( nd.eq.2 .and. jnp.gt.0 ) je1 = je1 - 1
       if ( nd.eq.3 .and. knp.gt.0) ke1 = ke1-1
       if ( nd.eq.3 .and. kpe.eq.ndz .and. knp.eq.0 ) ke1 = ke1-1

       do k = ks, ke1
        do j = js, je1
         do i = is, ie1

          inod = inod + 1

          edge_id_lc(nxi+ndepth+i,j,k,nd) =  inod
          node_id_gl = ioff_gl + i + (joff+j)*(ie1-is+1)                &
     &               + (koff+k-1)*(ie1-is+1)*ny_all

          if (nd .eq. 1) then
           x = xmax + ( dble(i+ndepth)-half )*xsize / dble(nx_all)
           y = yoff + dble(j-1) *             ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 2) then
           x = xmax + dble(i+ndepth-1) *     xsize / dble(nx_all)
           y = yoff + (dble(j)-half) *     ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 3) then
           x = xmax + dble(i+ndepth-1) *     xsize / dble(nx_all)
           y = yoff + dble(j-1) *            ysize / dble(ny_all)
           z = zz_edge(koff+k)
          end if

           write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z
!
         end do
        end do
       end do
!
       end subroutine set_sleeve_edge_xmax
!
!  ---------------------------------------------------------------------
!
!      subroutine set_sleeve_edge_ymin
!
       subroutine set_sleeve_edge_ymin(kpe, inp, knp,                   &
     &          inod, ioff_gl, nd)
!
!      Written by Kemorin
!
!
       integer (kind = kint) :: kpe, inp, knp
       integer (kind = kint) :: ioff_gl
       integer (kind = kint) :: inod
       integer (kind = kint) :: nd
!
       integer (kind= kint) :: node_id_gl
       integer (kind = kint) ::ie1, ke1
       integer (kind= kint) :: i, j, k
       real (kind= kreal) :: x, y, z
       real (kind = kreal), parameter :: half = 0.5d0
!
!
       ie1 = ie
       ke1 = ke
       if ( nd.eq.1 .and. inp.gt.0 ) ie1 = ie1 - 1
       if ( nd.eq.3 .and. knp.gt.0) ke1 = ke1-1
       if ( nd.eq.3 .and. kpe.eq.ndz .and. knp.eq.0 ) ke1 = ke1-1

       do k = ks, ke1
        do j = js, je
         do i = is, ie1

          inod = inod + 1

          edge_id_lc(i,j,k,nd) =  inod
          node_id_gl = ioff_gl + (ioff+i) + (j-1)*nx_all                &
     &               + (koff+k-1)*(je-js+1)*nx_all

          if (nd .eq. 1) then
           x = xoff + ( dble(i)-half )*xsize / dble(nx_all)
           y = ymin + dble(j-1) *        ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 2) then
           x = xoff + dble(i-1) *        xsize / dble(nx_all)
           y = ymin + ( dble(j)-half ) *   ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 3) then
           x = xoff + dble(i-1) *        xsize / dble(nx_all)
           y = ymin + dble(j-1) *        ysize / dble(ny_all)
           z = zz_edge(koff+k)
          end if

           write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z
!
         end do
        end do
       end do
!
       end subroutine set_sleeve_edge_ymin
!
!  ---------------------------------------------------------------------
!
!      subroutine set_sleeve_edge_ymax
!
       subroutine set_sleeve_edge_ymax(kpe, inp, knp,                   &
     &          inod, ioff_gl, nd)

!
!      Written by Kemorin
!
       integer (kind = kint) :: kpe, inp, knp
       integer (kind = kint) :: ioff_gl
       integer (kind = kint) :: inod
       integer (kind = kint) :: nd
!
       integer (kind = kint) :: node_id_gl
       integer (kind = kint) ::ie1, je1, ke1
       integer (kind = kint) :: i, j, k
       real (kind = kreal) :: x, y, z
       real (kind = kreal), parameter :: half = 0.5d0
!
!
       ie1 = ie
       je1 = je
       ke1 = ke
       if ( nd.eq.1 .and. inp.gt.0 ) ie1 = ie1 - 1
       if ( nd.eq.2 ) je1 = je1-1
       if ( nd.eq.3 .and. knp.gt.0) ke1 = ke1-1
       if ( nd.eq.3 .and. kpe.eq.ndz .and. knp.eq.0 ) ke1 = ke1-1

       do k = ks, ke1
        do j = js, je1
         do i = is, ie1

          inod = inod + 1

          edge_id_lc(i,nyi+ndepth+j,k,nd) =  inod
          node_id_gl = ioff_gl + (ioff+i) + (j-1)*nx_all                &
     &               + (koff+k-1)*(je1-js+1)*nx_all

          if (nd .eq. 1) then
           x = xoff + ( dble(i)-half )*         xsize / dble(nx_all)
           y = ymax + dble(ndepth+j-1) *          ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 2) then
           x = xoff + dble(i-1) *               xsize / dble(nx_all)
           y = ymax + ( dble(ndepth+j)-half ) * ysize / dble(ny_all)
           z = zz(koff+k)
          else if (nd .eq. 3) then
           x = xoff + dble(i-1) *               xsize / dble(nx_all)
           y = ymax + dble(ndepth+j-1) *        ysize / dble(ny_all)
           z = zz_edge(koff+k)
          end if

           write(l_out,'(i15,3(1pe21.11))') node_id_gl, x, y, z
!
         end do
        end do
       end do
!
       end subroutine set_sleeve_edge_ymax
!
!  ---------------------------------------------------------------------
!
      end module m_sleeve_edge_side_cube
