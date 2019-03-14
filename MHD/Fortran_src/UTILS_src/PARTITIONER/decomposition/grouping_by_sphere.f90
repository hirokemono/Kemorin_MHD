!
!      module grouping_by_sphere
!      Written by H. Matsui
!
!      subroutine count_num_subdomain_4_cube(NP,                        &
!     &          numnod_local, ncore_local, nrest_local)
!
!      subroutine set_sphere_domain_list_l(NP, nnod, iradius, num_CMB,  &
!     &          nnod_CMB, num_layer, istack_sph, item_sph,             &
!     &          IGROUP_cmb, IGROUP_radius, IGROUP, ncore_local)
!      subroutine set_sphere_domain_list_q(NP, nnod, iradius, num_CMB,  &
!     &          num_layer, istack20_sph, item20_sph,                   &
!     &          IGROUP_cmb, IGROUP_radius, IGROUP, ncore_local)
!
      module grouping_by_sphere
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
      subroutine count_num_subdomain_4_cube(NP,                         &
     &          numnod_local, ncore_local, nrest_local)
!
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: NP
      integer(kind = kint), intent(in) :: numnod_local(NP)
      integer(kind = kint), intent(in) :: ncore_local(NP)
!
      integer(kind = kint), intent(inout) :: nrest_local(NP)
!
      integer(kind = kint) :: ip, NP_posi
      integer(kind = kint) :: nconvert, num1, irest1
!
!
      do ip = 1, NP
        nrest_local(ip) = numnod_local(ip) - ncore_local(ip)
      end do
!
!      write(*,*) 'nrest_local a', nrest_local
!
      NP_posi = NP
      nconvert = 0
      do ip = 1, NP
        if (nrest_local(ip) .le. 0) then
          nconvert= nconvert-nrest_local(ip)
          nrest_local(ip) = 0
          NP_posi = NP_posi - 1
        end if
      end do
!
      if (NP_posi .gt. 0) then
        call cal_divide_and_rest(num1, irest1, nconvert, NP_posi)
      end if
!
      write(*,*) 'nrest_local c', nrest_local
      NP_posi = 0
      do ip = 1, NP
        if (nrest_local(ip) .gt. 0) then
          NP_posi = NP_posi + 1
          if (NP_posi.le.irest1) then
            nrest_local(ip) = nrest_local(ip) - num1 -1
          else
            nrest_local(ip) = nrest_local(ip) - num1
          end if
        end if
      end do
!
!      write(*,*) 'nrest_local'
!      write(*,*) nrest_local
!
      end subroutine count_num_subdomain_4_cube
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_sphere_domain_list_l(NP, nnod, iradius, num_CMB,   &
     &          nnod_CMB, num_layer, istack_sph, item_sph,              &
     &          IGROUP_cmb, IGROUP_radius, IGROUP, ncore_local)
!
      integer(kind = kint), intent(in) :: NP, iradius
      integer(kind = kint), intent(in) :: num_CMB, nnod_CMB
      integer(kind = kint), intent(in) :: nnod, num_layer
      integer(kind = kint), intent(in) :: istack_sph(nnod_CMB)
      integer(kind = kint), intent(in) :: item_sph(num_layer,nnod_CMB)
      integer(kind = kint), intent(in) :: IGROUP_cmb(num_CMB)
      integer(kind = kint), intent(in) :: IGROUP_radius(num_layer)
!
      integer(kind = kint), intent(inout) :: IGROUP(nnod)
      integer(kind = kint), intent(inout) :: ncore_local(NP)
!
      integer(kind = kint) :: inod, is, iss, id
!
      ncore_local = 0
      do is = 1, nnod_CMB
        do iss = 1, istack_sph(is)
          inod = item_sph(iss,is)
          id = (IGROUP_cmb(is)-1)*iradius + IGROUP_radius(iss)
          IGROUP(inod) = id
          ncore_local(id) = ncore_local(id) + 1
        end do
      end do
!
      end subroutine set_sphere_domain_list_l
!
!   --------------------------------------------------------------------
!
      subroutine set_sphere_domain_list_q(NP, nnod, iradius, num_CMB,   &
     &          num_layer, istack20_sph, item20_sph,                    &
     &          IGROUP_cmb, IGROUP_radius, IGROUP, ncore_local)
!
      integer(kind = kint), intent(in) :: NP, iradius
      integer(kind = kint), intent(in) :: nnod, num_CMB, num_layer
      integer(kind = kint), intent(in) :: istack20_sph(num_CMB)
      integer(kind = kint), intent(in) :: item20_sph(num_layer,num_CMB)
      integer(kind = kint), intent(in) :: IGROUP_cmb(num_CMB)
      integer(kind = kint), intent(in) :: IGROUP_radius(num_layer)
!
      integer(kind = kint), intent(inout) :: IGROUP(nnod)
      integer(kind = kint), intent(inout) :: ncore_local(NP)
!
      integer(kind = kint) :: inod, is, iss, id
!
      do is = 1, num_CMB
        do iss = 1, istack20_sph(is)
          inod = item20_sph(iss,is)
          id = (IGROUP_cmb(is)-1)*iradius + IGROUP_radius(iss)
          IGROUP(inod) = id
          ncore_local(id) = ncore_local(id) + 1
        end do
      end do
!
      end subroutine set_sphere_domain_list_q
!
!   --------------------------------------------------------------------
!
      end module grouping_by_sphere
