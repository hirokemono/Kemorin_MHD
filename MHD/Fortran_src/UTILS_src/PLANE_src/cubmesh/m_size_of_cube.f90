!
!     module m_size_of_cube
!
!     modified by Kemorin
!
      module m_size_of_cube
!
      use m_precision
!
      implicit none
!
! ......................................................................
!  * parameters for depth of sleeve area
!      ndepth     : depth of sleeve area
      integer(kind=kint )   ::  ndepth = 1
      integer(kind=kint )   ::  ndep_1 = 3
      integer(kind=kint )   ::  ndep_2 = 9
      integer(kind=kint )   ::  ndep_3 = 27
!  * variables

      integer(kind=kint )  ::  nxi, nyi, nzi  
! \beginLOCALVARS[nxi]     internal node count at x direction line
!       \LOCALVAR[nyi]     internal node count at y direction line
!       \LOCALVAR[nzi]     internal node count at z direction line

      integer(kind=kint )  ::  nx , ny , nz 
! \beginLOCALVARS[nx]      nodal count for x direction
!       \LOCALVAR[ny]      nodal count for y direction
!       \LOCALVAR[nz]      nodal count for z direction

      integer(kind=kint )  ::  nx1 , ny1, nz1 
! \beginLOCALVARS[nx1]      nodal count with 1 fill-in for x direction
!       \LOCALVAR[ny1]      nodal count with 1 fill-in for y direction
!       \LOCALVAR[nz1]      nodal count with 1 fill-in for z direction

      integer(kind=kint )  ::  numnod_x , numnod_y , numnod_z 
      integer(kind=kint )  ::  nnod_cubmesh
! \beginLOCALVARS[nx]      array size for x direction
!       \LOCALVAR[ny]      array size for y direction
!       \LOCALVAR[nz]      array size for z direction

      integer(kind=kint )  ::  nod_gltot
!   number of node in the domain without sleeves
      integer(kind=kint )  ::  edge_gltot
!   number of edge in the domain without sleeves
!
      integer(kind=kint )  ::  nodtot, intnodtot
      integer(kind=kint )  ::  elmtot, intelmtot
      integer(kind=kint )  ::  edgetot, intedgetot
!
      integer(kind=kint )  ::  elm_fil1_tot
!
!
      integer(kind = kint) :: iflag_filter = -1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine set_size_of_domain(elm_type, kpe)
!
       use m_size_4_plane
!
       integer (kind = kint) :: elm_type, kpe
!
!
!                                       .. pe nod per 1 line

                             nx  = nxi + 2*ndepth
                             nx1 = nxi + 2

                             ny  = nyi + 2*ndepth
                             ny1 = nyi + 2

                             nz  = nzi
                             nz1 = nzi
            if (kpe /=   1)  nz  = nz  + ndepth
            if (kpe /=   1)  nz1 = nz1 + 1
            if (kpe /= ndz)  nz  = nz  + ndepth
            if (kpe /= ndz)  nz1 = nz1 + 1

       if (elm_type .eq. 331) then
!                                       .. total node
            nodtot    = nx  * ny  * nz
!                                       .. internal nodes
            intnodtot = nxi * nyi * nzi
!
       else if (elm_type .eq. 332) then
!                                       .. total node
            nodtot    = nx  * ny  * nz
            edgetot   = (nx-1) * ny  * nz
            edgetot   = edgetot + nx * (ny-1)  * nz
            edgetot   = edgetot + nx * ny  * (nz-1)
            nodtot    = nodtot + edgetot
!
!                                       .. internal nodes
            intnodtot = nxi * nyi * nzi
            intnodtot = intnodtot + nxi * nyi * nzi
            intnodtot = intnodtot + nxi * nyi * nzi
            intnodtot = intnodtot + nxi * nyi * nzi
            if (kpe == ndz) intnodtot = intnodtot - nxi * nyi
!
       end if
!
       end subroutine set_size_of_domain
!
! ----------------------------------------------------------------------
!
      end module m_size_of_cube
