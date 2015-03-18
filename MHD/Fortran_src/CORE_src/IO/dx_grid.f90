!
!      module dx_grid
!
!      Written by H. Matsui on Feb., 2007
!      Modified by H. Matsui on May., 2009
!
!      subroutine write_dx_grid(nsize_nod, nsize_ele, nnod_ele,         &
!     &          nnod, nele, xx, ie, id_output,                         &
!     &          dx_node_fname, dx_conn_fname)
!
      module dx_grid
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_dx_grid(nsize_nod, nsize_ele, nnod_ele,          &
     &          nnod, nele, xx, ie, id_output,                          &
     &          dx_node_fname, dx_conn_fname)
!
      use m_geometry_constants
      use m_connect_hexa_2_tetra
!
      integer(kind = kint), intent(in) :: nsize_nod, nsize_ele
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(in) :: ie(nsize_ele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nsize_nod,3)
!
      integer(kind = kint), intent(in) ::  id_output
      character(len=kchara), intent(in) :: dx_node_fname
      character(len=kchara), intent(in) :: dx_conn_fname
!
      integer(kind = kint) :: nd, i, j
      integer(kind = kint) :: numele_quad, nsegment_quad
      integer(kind = kint) :: inod, iele
      integer(kind = kint) :: iq(8)
!
!
      if (nnod_ele .eq. num_t_linear) then
        nsegment_quad = 5
        numele_quad = 5*nele
        call set_1_hexa_2_5_tetra
      else if (nnod_ele .eq. num_t_quad) then
        nsegment_quad = 21
        numele_quad = 20*nele
        call set_1_hexa_2_21_tetra
      else if (nnod_ele .eq. num_t_lag) then
        nsegment_quad = 40
        numele_quad = 40*nele
        call set_1_hexa_2_40_tetra
      else if (nnod_ele .eq. num_triangle) then
        nsegment_quad = 1
        numele_quad = nele
      else if (nnod_ele .eq. num_linear_edge) then
        nsegment_quad = 1
        numele_quad = nele
      end if
!
!
      write(*,*) 'merged grid data:     ', trim(dx_node_fname)
      open (id_output,  file=dx_node_fname,                             &
     &    form='formatted', status ='unknown')
!
      do inod = 1, nnod
       write(id_output,'(1p3E25.15e3)') (xx(inod,nd), nd= 1, 3)
      end do
!
      close (id_output)
!
!
!
      write(*,*) 'merged grid data:     ', trim(dx_conn_fname)
      open (id_output,  file=dx_conn_fname,                             &
     &    form='formatted', status ='unknown')
!
      do iele = 1, nele
        if (nnod_ele.eq.num_t_quad .or. nnod_ele.eq.num_t_lag) then
          do j = 1, nsegment_quad
            do i = 1, 4
              iq(i) = ie(iele,ie_tetra(i,j))-1
            end do
            write(id_output,'(4i16)') (iq(nd),nd= 1,4)
          end do
        else if (nnod_ele .eq. num_t_linear) then
          iq(1) = ie(iele,4) - 1
          iq(2) = ie(iele,1) - 1
          iq(3) = ie(iele,8) - 1
          iq(4) = ie(iele,5) - 1
          iq(5) = ie(iele,3) - 1
          iq(6) = ie(iele,2) - 1
          iq(7) = ie(iele,7) - 1
          iq(8) = ie(iele,6) - 1
          write(id_output,'(8i16)') (iq(nd),nd= 1,nnod_ele)
        else if (nnod_ele .eq. num_triangle) then
          iq(1) = ie(iele,1) - 1
          iq(2) = ie(iele,2) - 1
          iq(3) = ie(iele,3) - 1
          write(id_output,'(3i16)') (iq(nd),nd= 1,nnod_ele)
        else if (nnod_ele .eq. num_linear_edge) then
          iq(1) = ie(iele,1) - 1
          iq(2) = ie(iele,2) - 1
          write(id_output,'(3i16)') (iq(nd),nd= 1,nnod_ele)
        end if
      end do
!
      close(id_output)
      if (nnod_ele .ne. num_triangle) call deallocate_hex_2_tetra
!
      end subroutine write_dx_grid
!
! -----------------------------------------------------------------------
!
      end module dx_grid
