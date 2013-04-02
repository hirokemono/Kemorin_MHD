!
!
      program add_elegrp_sph

      use m_precision
!
      use analyzer_add_ele_group
!
      implicit none
!
      call initialize_add_egrp
      call  analyze_add_egrp

      stop
      end program add_elegrp_sph
