!set_comm_edge_4_cube.f90
!     module set_comm_edge_4_cube
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!      subroutine count_im_edge(kpe, inp, jnp, knp, inod, nd)
!      subroutine set_im_edge(kpe, inp, jnp, knp, inod, nd)
!
!      subroutine count_ex_edge(kpe, inp, jnp, knp, inod, nd)
!      subroutine set_ex_edge(kpe, inp, jnp, knp, inod, nd)
!
      module set_comm_edge_4_cube
!
      use m_precision
!
      use m_size_4_plane
      use m_size_of_cube
      use m_comm_data_cube_kemo
      use m_sleeve_cube
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine count_im_edge(kpe, inp, jnp, knp, inod, nd)
!
      integer (kind = kint), intent(in) :: kpe, inp, jnp, knp
      integer (kind = kint), intent(in) :: nd
      integer (kind = kint), intent(inout) :: inod
!
!
      if (nd.eq.1 .and. inp.eq. 1 ) then
        inod = inod + (ie-is  )*(je-js+1)*(ke-ks+1)
      else if (nd.eq.2 .and. jnp.eq. 1 ) then
        inod = inod + (ie-is+1)*(je-js  )*(ke-ks+1)
      else if (nd.eq.3 .and. kpe.eq.ndz .and. knp.eq. 0) then
        inod = inod + (ie-is+1)*(je-js+1)*(ke-ks  )
      else if (nd.eq.3 .and.  knp.eq. 1 ) then
        inod = inod + (ie-is+1)*(je-js+1)*(ke-ks  )
      else
        inod = inod + (ie-is+1)*(je-js+1)*(ke-ks+1)
      end if
!
      end subroutine count_im_edge
!
! ----------------------------------------------------------------------
!
       subroutine set_im_edge(kpe, inp, jnp, knp, inod, nd)
!
      use m_local_node_id_cube
!
      integer (kind = kint) :: kpe, inp, jnp, knp
      integer (kind = kint) :: inod
      integer (kind = kint) :: nd
!
      integer (kind = kint) :: ie1, je1, ke1
      integer (kind = kint) :: i, j, k
!
!
      ie1 = ie
      je1 = je
      ke1 = ke
      if ( nd.eq.1 .and. inp.eq. 1 ) ie1 = ie1-1
      if ( nd.eq.2 .and. jnp.eq. 1 ) je1 = je1-1
      if ( nd.eq.3 .and. knp.eq. 1 ) ke1 = ke1-1
      if ( nd.eq.3 .and. kpe.eq.ndz .and. knp.eq. 0) ke1 = ke1-1
!
      do k=ks,ke1
        do j=js,je1
          do i=is,ie1

            inod = inod + 1
            item_import(inod) =  edge_id_lc(i,j,k,nd)

          enddo
        enddo
      enddo
!
      end subroutine set_im_edge
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_ex_edge(kpe, inp, jnp, knp, inod, nd)
!
      integer (kind = kint), intent(in) :: kpe, inp, jnp, knp
      integer (kind = kint), intent(in) :: nd
      integer (kind = kint), intent(inout) :: inod
!
!
      if (nd.eq.1 .and. inp.eq. -1 ) then
       inod = inod + (ie-is  )*(je-js+1)*(ke-ks+1)
      else if (nd.eq.2 .and. jnp.eq. -1 ) then
       inod = inod + (ie-is+1)*(je-js  )*(ke-ks+1)
      else if (nd.eq.3 .and. kpe.eq.ndz .and. knp.eq. 0) then
       inod = inod + (ie-is+1)*(je-js+1)*(ke-ks  )
      else if (nd.eq.3 .and.  knp.eq. -1 ) then
       inod = inod + (ie-is+1)*(je-js+1)*(ke-ks  )
      else
       inod = inod + (ie-is+1)*(je-js+1)*(ke-ks+1)
      end if
!
      end subroutine count_ex_edge
!
! ----------------------------------------------------------------------
!
       subroutine set_ex_edge(kpe, inp, jnp, knp, inod, nd)

      use m_local_node_id_cube
!
      integer (kind = kint) :: kpe, inp, jnp, knp
      integer (kind = kint) :: inod
      integer (kind = kint) :: nd
!
      integer (kind = kint) :: ie1, je1, ke1
      integer (kind = kint) :: i, j, k
!
!
      ie1 = ie
      je1 = je
      ke1 = ke
      if ( nd.eq.1 .and. inp.eq. -1 ) ie1 = ie1-1
      if ( nd.eq.2 .and. jnp.eq. -1 ) je1 = je1-1
      if ( nd.eq.3 .and. knp.eq. -1 ) ke1 = ke1-1
      if ( nd.eq.3 .and. kpe.eq.ndz .and. knp.eq. 0) ke1 = ke1-1
      do k=ks,ke1
        do j=js,je1
          do i=is,ie1

            inod = inod + 1
            item_export(inod) =  edge_id_lc(i,j,k,nd)

          enddo
        enddo
      enddo
!
      end subroutine set_ex_edge
!
! ----------------------------------------------------------------------
!
      end module set_comm_edge_4_cube
