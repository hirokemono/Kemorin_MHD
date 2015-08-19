!
!     module set_bin_id_4_destination
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine s_set_bin_id_4_destination(dest_node, id_search_area)
!
      module set_bin_id_4_destination
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_bin_id_4_destination(dest_node, id_search_area)
!
      use t_geometry_data
      use m_sphere_bin_4_table
!
      type(node_data), intent(in) :: dest_node
      integer(kind = kint), intent(inout)                               &
     &           :: id_search_area(dest_node%internal_node,3)
!
      integer(kind = kint) :: ip, ist, ied
      integer(kind = kint) :: inod, j, ihash
      integer(kind = kint) :: jr, jt, jp
      integer(kind = kint) :: jr_bin, jt_bin, jp_bin
      integer(kind = kint) :: jr_st, jt_st, jp_st
!
!
!$omp parallel do private (ist,ied,inod,j,jr,jt,jp,jr_st,jt_st,jp_st,   &
!$omp& jr_bin,jt_bin,jp_bin)
      do ip = 1, np_smp
        ist = dest_node%istack_internal_smp(ip-1) + 1
        ied = dest_node%istack_internal_smp(ip)
!
        jp_st = 0
        jt_st = 0
        jr_st = 0
        do inod = ist, ied
!
          do j = 1, num_sph_grid(3)
            jp = mod( (jp_st+j-ione), num_sph_grid(3) ) + ione
            if (    dest_node%phi(inod) .ge. phi_divide(jp-1)           &
     &        .and. dest_node%phi(inod) .lt. phi_divide(jp) ) then
              jp_bin = jp
              jp_st = jp - 1
              exit
            end if
            if (j .eq. num_sph_grid(3)) then
              jp_bin = jp
              jp_st = jp - 1
            end if
          end do
!
          do j = 1, num_sph_grid(2)
            jt = mod( (jt_st+j-ione), num_sph_grid(2) ) + ione
            if (    dest_node%theta(inod) .ge. theta_divide(jt-1)       &
     &        .and. dest_node%theta(inod) .lt. theta_divide(jt) ) then
              jt_bin = jt
              jt_st = jt - 1
              exit
            end if
            if (j .eq. num_sph_grid(2)) then
              jt_bin = jt
              jt_st = jt - 1
            end if
          end do
!
          do j = 1, num_sph_grid(1)+1
            jr = mod( (jr_st+j-ione), (num_sph_grid(1)+1) ) + ione
            if ( jr.eq. num_sph_grid(1)+1 ) then
              if (dest_node%rr(inod) .ge. r_divide(num_sph_grid(1)))    &
     &         then
                jr_bin = jr
                jr_st = jr - 1
                exit
              end if
            else if ( dest_node%rr(inod) .ge. r_divide(jr-1)            &
     &          .and. dest_node%rr(inod) .lt. r_divide(jr) ) then
                jr_bin = jr
                jr_st = jr - 1
                exit
            end if
          end do
!
          id_search_area(inod,1) = jr_bin
          id_search_area(inod,2) = jt_bin
          id_search_area(inod,3) = jp_bin
          ihash = jr_bin                                                &
     &           + (jp_bin - 1) * num_sph_bin(1)                        &
     &           + (jt_bin - 1) * num_sph_bin(1) * num_sph_bin(3)
!
          if (ihash.le.0 .or. ihash.gt.ntot_sph_bin) then
             write(*,*) 'inod, jr_bin, jt_bin, jp_bin'
             write(*,*) inod, jr_bin, jt_bin, jp_bin
             write(*,*) 'position'
             write(*,*) dest_node%rr(inod), dest_node%theta(inod),      &
     &                  dest_node%phi(inod)
          end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine s_set_bin_id_4_destination
!
!  ---------------------------------------------------------------------
!
      end module set_bin_id_4_destination
